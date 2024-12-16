module OCamlPrettyPrinter where

import Data.List
import Test.QuickCheck as QC
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Control.Applicative (liftA2)
import OCamlSyntax

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: (PP a) => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP Value where
  pp :: Value -> Doc
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp (TupleVal t) = PP.parens $ printList t
  pp (ListVal l) = PP.brackets $ printList l
  pp (FunctionVal i e) = PP.parens $ PP.text "fun" <+> pp i <+> PP.text "->" <+> pp e

instance PP (Either String Expression) where
  pp :: Either String Expression -> Doc
  pp (Left s) = PP.text $ "(!) " ++ s
  pp (Right e) = pp e

instance PP Uop where
  pp Neg = PP.char '-'
  pp Not = PP.text "not"

instance PP Bop where
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.char '/'
  pp Mod = PP.text "mod"
  pp Eq = PP.char '='
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Append = PP.char '@'
  pp Cons = PP.text "::"
  pp Or = PP.text "||"
  pp And = PP.text "&&"

instance PP Bool where
  pp :: Bool -> Doc
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP Int where
  pp :: Int -> Doc
  pp = PP.int

instance PP String where
  pp :: String -> Doc
  pp = PP.text

printList :: [Value] -> Doc
printList [] = PP.text ""
printList [x] = pp x
printList (x : xs) = pp x <> PP.text ", " <> printList xs


instance PP Expression where
  pp :: Expression -> Doc
  pp (Var i) = pp i
  pp (Val v) = pp v
  pp (Op1 Neg e) = PP.char '-' <> PP.parens (pp e)
  pp (Op1 Not e) = PP.text "not" <+> PP.parens (pp e)
  pp (Op2 e1 op e2) = PP.parens (pp e1) <+> pp op <+> PP.parens (pp e2)
  pp (ListConst l) = PP.brackets $ PP.hcat (PP.punctuate (PP.text "; ") (pp <$> l))
  pp (TupleConst [e]) = pp e
  pp (TupleConst l) = PP.parens $ PP.hcat (PP.punctuate (PP.text ", ") (pp <$> l))
  pp (FunctionConst i e) = PP.text "fun" <+> pp i <+> PP.text "->" <+> pp e
  pp (If e1 e2 e3) = 
    PP.text "if" <+> pp e1 PP.$$
    PP.nest 4 (PP.text "then" <+> pp e2 PP.$$ PP.text "else" <+> pp e3)
  pp (Match e l) =
    PP.text "begin match" <+> pp e <+> PP.text "with" PP.$$
    PP.nest 4 (printList l)
    where
      printList :: [(Pattern, Expression)] -> Doc
      printList [] = PP.text "end"
      printList ((p, e) : xs) =
        PP.vcat $
          (PP.char '|' <+> pp p <+> PP.text "->" <+> pp e) : [printList xs]
  pp (Let i e1 e2) = PP.text "let" <+> pp i <+> PP.char '=' <+> pp e1 <+> PP.text "in" PP.$$ PP.nest 4 (pp e2)
  pp (Apply f a) = PP.parens (PP.parens (pp f) <+> PP.parens (pp a))

instance PP Pattern where
  pp :: Pattern -> Doc
  pp (IntConstPat i) = pp i
  pp (BoolConstPat b) = pp b
  pp (IdentifierPat i) = pp i
  pp (ListPat l) = PP.brackets $ PP.hcat (PP.punctuate (PP.char ';') (pp <$> l))
  pp (ConsPat p1 p2) = PP.parens (pp p1 <> PP.text "::" <> pp p2)
  pp (TuplePat l) = PP.parens $ PP.hcat (PP.punctuate (PP.char ',') (pp <$> l))
  pp WildcardPat = PP.char '_'

instance PP Statement where
  pp :: Statement -> Doc
  pp (VarDecl b i e) = PP.text "let" <> eval b <> PP.space <> pp i <> PP.text " = " <> pp e where
    eval :: Bool -> Doc
    eval b = if b then PP.text " rec" else PP.text ""
  pp Empty = PP.char ';'

-- Arbitrary Instances

genId :: Gen Identifier
genId = QC.elements ["x", "X", "y", "x0", "X0", "xy", "XY"]

genVal :: Int -> Gen Value
genVal 0 = QC.oneof [IntVal <$> arbitrary, BoolVal <$> arbitrary]
genVal n =
  QC.frequency
    [
      (n, IntVal <$> arbitrary),
      (n, BoolVal <$> arbitrary)
    ] where
        n' = n `div` 2

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = QC.sized genVal
  shrink :: Value -> [Value]
  shrink i@(IntVal _i) = return i
  shrink b@(BoolVal _b) = return b
  shrink _ = []


genExpList :: Int -> Gen [Expression]
genExpList n = do
  len <- QC.elements [2 .. 3]
  take len <$> QC.infiniteListOf (genExp n)

genPatExpList :: Int -> Gen [(Pattern, Expression)]
genPatExpList n = do
  len <- QC.elements [1 .. 3]
  take len <$> QC.infiniteListOf (liftA2 (,) (genPat n) (genExp n))

genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [Var <$> genId, Val <$> arbitrary]
genExp n =
  QC.frequency
    [
      (1, Var <$> genId),
      (n `min` 10, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
      (n, ListConst <$> genExpList n'),
      (n, TupleConst <$> genExpList n'),
      (n, (Match . Var <$> genId) <*> genPatExpList n'),
      (n, If <$> genExp n' <*> genExp n' <*> genExp n'),
      (n, Let <$> genId <*> genExp n' <*> genExp n'),
      (n, Apply <$> genExp n' <*> genExp n')
    ] where
        n' = n `div` 2

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = QC.sized genExp
  shrink :: Expression -> [Expression]
  shrink (Var v) = [] -- No shrinking for variables
  shrink (Val v) = Val <$> shrink v -- Shrink values if possible
  shrink (Op1 o e) = [e] -- Shrink to the subexpression
  shrink (Op2 e1 o e2) = [e1, e2] -- Shrink to subexpressions
  shrink (ListConst l) = [] -- Avoid recursive shrinking of lists
  shrink (TupleConst l) = [] -- Avoid recursive shrinking of tuples
  shrink (FunctionConst i e) = [e] -- Shrink to the body
  shrink (If e b1 b2) = [b1, b2] -- Shrink to branches only
  shrink (Match e l) = [e] -- Shrink to the expression only
  shrink (Let i e1 e2) = [e1, e2] -- Shrink to subexpressions
  shrink (Apply e1 e2) = [e1, e2] -- Shrink to subexpressions


genPatList :: Int -> Gen [Pattern]
genPatList n = do
  take 2 <$> QC.infiniteListOf (genPat n)

genPat :: Int -> Gen Pattern
genPat 0 = QC.oneof [IntConstPat <$> arbitrary, BoolConstPat <$> arbitrary]
genPat n =
  QC.frequency
    [
      (1, IntConstPat <$> arbitrary),
      (1, BoolConstPat <$> arbitrary),
      (1, return WildcardPat),
      (n, IdentifierPat <$> genId),
      (n, ListPat <$> genPatList n'),
      (n, ConsPat <$> genPat n' <*> genPat n'),
      (n, TuplePat <$> genPatList n')
    ] where
        n' = n `div` 2

instance Arbitrary Pattern where
  arbitrary :: Gen Pattern
  arbitrary = QC.sized genPat
  shrink :: Pattern -> [Pattern]
  shrink (IntConstPat i) = IntConstPat <$> shrink i
  shrink (BoolConstPat b) = BoolConstPat <$> shrink b
  shrink id@(IdentifierPat i) = [id]
  shrink (ListPat l) = [ListPat l' | l' <- shrink l]
  shrink (ConsPat p1 p2) = p1 : p2
    : [ConsPat p1' p2 | p1' <- shrink p1]
    ++ [ConsPat p1 p2' | p2' <- shrink p2]
  shrink (TuplePat l) = [TuplePat l' | l' <- shrink l]
  shrink WildcardPat = return WildcardPat


instance Arbitrary Uop where
  arbitrary :: Gen Uop
  arbitrary = QC.arbitraryBoundedEnum


instance Arbitrary Bop where
  arbitrary :: Gen Bop
  arbitrary = QC.arbitraryBoundedEnum


genStatement :: Int -> Gen Statement
genStatement n = VarDecl <$> arbitrary <*> genId <*> genExp n
  where
    n' = n `div` 2

instance Arbitrary Statement where
  arbitrary :: Gen Statement
  arbitrary = QC.sized genStatement
  shrink :: Statement -> [Statement]
  shrink Empty = return Empty
  shrink s = [s]


genBlock :: Int -> Gen Block
genBlock n = Block <$> genStmts n where
  genStmts 0 = pure []
  genStmts n =
    QC.frequency
      [
        (1, pure []),
        (n, (:) <$> genStatement n' <*> genStmts n')
      ] where
        n' = n `div` 2

instance Arbitrary Block where
  arbitrary :: Gen Block
  arbitrary = QC.sized genBlock
  shrink :: Block -> [Block]
  shrink (Block ss) = [Block ss' | ss' <- shrink ss]
