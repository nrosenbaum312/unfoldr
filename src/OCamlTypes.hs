module OCamlTypes where

import OCamlSyntax

data Type
  = TInt
  | TBool
  | TTuple [Type]
  | TList Type
  | TFunction Type Type
  | TUnknown
  deriving (Eq, Show)

showType :: Value -> String
showType (IntVal _) = "int"
showType (BoolVal _) = "bool"
showType (TupleVal []) = "unit"
showType (TupleVal _) = "tuple"
showType (ListVal _) = "list"
showType (FunctionVal _ _) = "function"

typeof :: Value -> Type
typeof (IntVal _) = TInt
typeof (BoolVal _) = TBool
typeof (TupleVal l) = TTuple $ map typeof l
typeof (ListVal []) = TList TUnknown
typeof (ListVal (x : _)) = TList (typeof x)
typeof (FunctionVal _ _) = TFunction TUnknown TUnknown

typeofl :: [Value] -> Type
typeofl [] = TUnknown
typeofl (x : _) = typeof x