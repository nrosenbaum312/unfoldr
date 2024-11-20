import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
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
    pp Const = PP.text "::"
    pp Or = PP.text "||"
    pp And = PP.text "&&"

instance PP Bool where
    pp :: Bool -> Doc
    pp True = PP.text "true"
    pp False = PP.text "false"

instance PP Int where
    pp :: Int -> Doc
    pp = PP.int

instance PP Value where
    pp (IntVal i) = PP.int i
    pp (BoolVal b) = PP.bool b
    pp (TupleVal t) = undefined
    pp (ListVal l) = undefined
    pp (FunctionVal is e) = undefined

instance PP Expression where
    pp (Var i) = Pp.text i
    pp (Val v) = undefined
    pp (Op1 op e) = undefined
    pp (Op2 e1 op e2) = undefined
    pp (ListConst l) = undefined
    pp (FunctionConst l e) = undefined
    pp (If e1 e2 e3) = undefined
    pp (Match e l) = undefined
    pp (Let i e1 e2) = undefined

instance PP Pattern where
    pp (IntConstPat i) = undefined
    pp (BoolConstPat b) = undefined
    pp (IndentifierPat i) = undefined
    pp (ListPat l) = undefined
    pp (ConsPat p1 p2) = undefined
    pp (TuplePat l) = undefined
    pp WildcardPat = undefined

instance PP Statement where
    pp (FunctionDecl b i l e) = undefined
    pp (VarDecl i e) = undefined
    pp Empty = undefined