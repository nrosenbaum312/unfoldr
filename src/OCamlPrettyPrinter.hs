import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Data.List
import Test.QuickCheck as QC
import qualified Test.QuickCheck as QC
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

instance PP Value where
  pp :: Value -> Doc
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp (TupleVal t) = PP.parens $ PP.text (intercalate ", " (map show t))
  pp (ListVal l) = PP.brackets $ PP.text (intercalate ";" (map show l))
  pp (FunctionVal is e) = PP.text "fun" <+> PP.text (unwords (map show is)) <+> PP.text "->" <+> pp e

instance PP Expression where
  pp :: Expression -> Doc
  pp (Var i) = pp i
  pp (Val v) = pp v
  pp (Op1 Neg e) = PP.char '-' <> pp e
  pp (Op1 Not e) = PP.text "not" <+> pp e
  pp (Op2 e1 op e2) = pp e1 <+> pp op <+> pp e2
  pp (ListConst l) = PP.brackets $ PP.hcat (PP.punctuate (PP.char ',') (pp <$> l))
  pp (TupleConst l) = undefined
  pp (FunctionConst l e) = undefined
  pp (If e1 e2 e3) = undefined
  pp (Match e l) = undefined
  pp (Let i e1 e2) = undefined

instance PP Pattern where
  pp :: Pattern -> Doc
  pp (IntConstPat i) = undefined
  pp (BoolConstPat b) = undefined
  pp (IdentifierPat i) = undefined
  pp (ListPat l) = undefined
  pp (ConsPat p1 p2) = undefined
  pp (TuplePat l) = undefined
  pp WildcardPat = undefined

instance PP Statement where
  pp :: Statement -> Doc
  pp (FunctionDecl b i l e) = undefined
  pp (VarDecl i e) = undefined
  pp Empty = undefined

-- level :: Bop -> Int
-- level Times = 7
-- level Divide = 7
-- level Plus = 5
-- level Minus = 5
-- level Append = 4
-- level Cons = 4
-- level _ = 3 -- comparison and boolean operators

-- isBase :: Expression -> Bool
-- isBase TableConst {} = True
-- isBase Val {} = True
-- isBase Var {} = True
-- isBase Op1 {} = True
-- isBase _ = False

instance Arbitrary Block where
  arbitrary :: Gen Block
  arbitrary = undefined
  shrink :: Block -> [Block]
  shrink (Block ss) = undefined

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = 
    QC.oneof
      [
        IntVal <$> arbitrary,
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