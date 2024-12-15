module OCamlPrettyPrinter where

import Data.List
import OCamlSyntax
import Test.QuickCheck as QC
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

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
  pp (FunctionVal i e) = PP.text "fun" <+> pp i <+> PP.text "->" <+> pp e

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
  pp (Op2 e1 op e2) = pp e1 <+> pp op <+> pp e2
  pp (ListConst l) = PP.brackets $ PP.hcat (PP.punctuate (PP.text "; ") (pp <$> l))
  pp (TupleConst l) = PP.parens $ PP.hcat (PP.punctuate (PP.text ", ") (pp <$> l))
  pp (FunctionConst i e) = PP.text "fun" <+> pp i <+> PP.text "->" <+> pp e
  pp (If e1 e2 e3) = PP.text "if" <+> pp e1 <+> PP.text "then" <+> pp e2 <+> PP.text "else" <+> pp e3
  pp (Match e l) = PP.text "begin match" <+> pp e <+> PP.text "with" <+> printList l  where
    printList :: [(Pattern, Expression)] -> Doc
    printList [] = PP.text "end"
    printList ((p, e) : xs) = PP.char '|' <+> pp p <+> PP.text "->" <+> pp e PP.$+$ printList xs
  pp (Let i e1 e2) = PP.text "let" <+> pp i <+> PP.char '=' <+> pp e1 <+> PP.text "in" PP.$+$ pp e2
  pp (Apply f a) = pp f <+> pp a

instance PP Pattern where
  pp :: Pattern -> Doc
  pp (IntConstPat i) = pp i
  pp (BoolConstPat b) = pp b
  pp (IdentifierPat i) = pp i
  pp (ListPat l) = PP.brackets $ PP.hcat (PP.punctuate (PP.char ';') (pp <$> l))
  pp (ConsPat p1 p2) = pp p1 <> PP.text "::" <> pp p2
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

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary =
    QC.oneof
      [ IntVal <$> arbitrary,
        BoolVal <$> arbitrary
      ]
  shrink :: Value -> [Value]
  shrink i@(IntVal _i) = return i
  shrink b@(BoolVal _b) = return b
  shrink _ = []


genExpList :: Int -> Gen [Expression]
genExpList n = do
  len <- QC.elements [0 .. 3]
  take len <$> QC.infiniteListOf (genExp n)

genPatExpList :: Int -> Gen [(Pattern, Expression)]
genPatExpList n = do
  len <- QC.elements [0 .. 3]
  take len <$> QC.infiniteListOf (liftA2 (,) (genPat n) (genExp n))

genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [Var <$> genId, Val <$> arbitrary]
genExp n = 
  QC.frequency
    [
      (1, Var <$> genId),
      (1, Val <$> arbitrary),
      (n `min` 10, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n')
      -- (n, ListConst <$> genExpList n'),
      -- (n, TupleConst <$> genExpList n'),
      -- (n, FunctionConst <$> genId <*> genExp n'),
      -- (n, Match <$> genExp n' <*> genPatExpList n'),
      -- (n, Let <$> genId <*> genExp n' <*> genExp n'),
      -- (n, Apply <$> genExp n' <*> genExp n')
    ] where
        n' = n `div` 2

-- instance Arbitrary Expression where
--   arbitrary :: Gen Expression
--   arbitrary = QC.sized genExp
--   shrink :: Expression -> [Expression]
--   shrink va@(Var v) = [va]
--   shrink (Val v) = Val <$> shrink v
--   shrink (Op1 o e) = [Op1 o e] 
--   -- : [Op1 o e' | e' <- shrink e]
--   shrink (Op2 e1 o e2) = [e1, e2]
--   -- shrink (Op2 e1 o e2) = e1 : e2
--   --   : [Op2 e1' o e2 | e1' <- shrink e1]
--   --   ++ [Op2 e1 o e2' | e2' <- shrink e2]
--   shrink (ListConst l) = 
--     [ListConst l' | l' <- shrink l]
--   shrink (TupleConst l) = 
--     [TupleConst l' | l' <- shrink l]
--   shrink (FunctionConst i e) = e :
--     [FunctionConst i e' | e' <- shrink e]
--   shrink (If e b1 b2) = b1 : b2 
--     : [If e' b1 b2 | e' <- shrink e]
--     ++ [If e b1' b2 | b1' <- shrink b1]
--     ++ [If e b1 b2' | b2' <- shrink b2]
--   shrink (Match e l) = e :
--     [Match e' l | e' <- shrink e]
--     ++ [Match e l' | l' <- shrink l]
--   shrink (Let i e1 e2) = e1 : e2
--     : [Let i e1' e2 | e1' <- shrink e1]
--     ++ [Let i e1 e2' | e2' <- shrink e2]
--   shrink (Apply e1 e2) = e1 : e2
--     : [Apply e1' e2 | e1' <- shrink e1]
--     ++ [Apply e1 e2' | e2' <- shrink e2]

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
  len <- QC.elements [0 .. 3]
  take len <$> QC.infiniteListOf (genPat n)

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
genStatement n | n <= 1 = QC.oneof [VarDecl <$> arbitrary <*> arbitrary <*> genExp 0, return Empty]
genStatement n = 
  QC.frequency 
    [
      (1, return Empty),
      (2, VarDecl <$> arbitrary <*> arbitrary <*> genExp n')
    ] where
      n' = n `div` 2

instance Arbitrary Statement where
  arbitrary :: Gen Statement
  arbitrary = QC.sized genStatement
  shrink :: Statement -> [Statement]
  shrink Empty = return Empty
  shrink (VarDecl b i e) = 
    [VarDecl b' i e | b' <- shrink b]
    ++ [VarDecl b i e' | e' <- shrink e]


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

-- >>> pretty (Apply (Op1 Neg (Val (BoolVal False))) (Op1 Neg (Val (BoolVal False))))
-- "-(false) -(false)"
