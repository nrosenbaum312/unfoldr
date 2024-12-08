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

instance PP Value where
  pp :: Value -> Doc
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp (TupleVal t) = PP.parens $ printList t
  pp (ListVal l) = PP.brackets $ printList l
  pp (FunctionVal i e) = PP.text "fun" <+> pp i <+> PP.text "->" <+> pp e

instance PP Expression where
  pp :: Expression -> Doc
  pp (Val v) = pp v
  pp (Op1 Neg e) = PP.char '-' <> pp e
  pp (Op1 Not e) = PP.text "not" <+> pp e
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
  pp i = pp i

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
  pp (VarDecl b i e) = PP.text "let " <> eval b <> PP.space <> pp i <> PP.space <> PP.char '=' <> PP.space <> pp e where
    eval :: Bool -> Doc
    eval b = if b then PP.text "rec " else PP.text ""
  pp Empty = PP.char ';'

instance Arbitrary Block where
  arbitrary :: Gen Block
  arbitrary = undefined
  shrink :: Block -> [Block]
  shrink (Block ss) = undefined

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary =
    QC.oneof
      [ IntVal <$> arbitrary,
        BoolVal <$> arbitrary
      ]
  shrink :: Value -> [Value]
  shrink (IntVal i) = IntVal <$> shrink i
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink _ = []

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = undefined
  shrink :: Expression -> [Expression]
  shrink = undefined

instance Arbitrary Pattern where
  arbitrary :: Gen Pattern
  arbitrary = undefined
  shrink :: Pattern -> [Pattern]
  shrink = undefined

instance Arbitrary Uop where
  arbitrary :: Gen Uop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary :: Gen Bop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Statement where
  arbitrary :: Gen Statement
  arbitrary = undefined
  shrink :: Statement -> [Statement]
  shrink = undefined
