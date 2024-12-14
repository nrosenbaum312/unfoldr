import OCamlParser
import Control.Exception (PatternMatchFail)
import GHC.Base (many, undefined, (<|>))
import GHC.Generics (Par1)
import OCamlPrettyPrinter
import OCamlSyntax
import Parser as P
import Test.HUnit
import qualified Data.Char as Char
import System.IO (getContents)  -- For reading from standard input
import Test.QuickCheck as QC

--- quickCheck properties that ensures that parsing is the inverse of printing

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP (pretty s) == Right s

test_value :: Test
test_value =
  "parsing values"
    ~: TestList
      [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        P.parse tupleValP "(1, 2)" ~?= Right (TupleVal [IntVal 1, IntVal 2]),
        P.parse listValP "[1; 2; 3]" ~?= Right (ListVal [IntVal 1, IntVal 2, IntVal 3]),
        P.parse functionValP "fun x -> x + 1" ~?= Right (FunctionVal "x" (Op2 (Var "x") Plus (Val (IntVal 1))))
      ]

test_expression :: Test
test_expression =
  "parsing expressions"
    ~: TestList
      [ P.parse varP "x" ~?= Right (Var "x"),
        P.parse valP "42" ~?= Right (Val (IntVal 42)),
        P.parse op1P "-x" ~?= Right (Op1 Neg (Var "x")),
        P.parse op2P "x + 1" ~?= Right (Op2 (Var "x") Plus (Val (IntVal 1))),
        P.parse listConstP "[1; 2; 3]" ~?= Right (ListConst [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)]),
        P.parse tupleConstP "(1, x)" ~?= Right (TupleConst [Val (IntVal 1), Var "x"]),
        P.parse functionConstP "fun x y -> x + y"
          ~?= Right (FunctionConst "x" (FunctionConst "y" (Op2 (Var "x") Plus (Var "y")))),
        P.parse functionConstP "fun x -> fun y -> x + y"
          ~?= Right (FunctionConst "x" (FunctionConst "y" (Op2 (Var "x") Plus (Var "y")))),
        P.parse ifP "if true then 1 else 0"
          ~?= Right (If (Val (BoolVal True)) (Val (IntVal 1)) (Val (IntVal 0))),
        P.parse letP "let x = 1 in x + 1"
          ~?= Right (Let "x" (Val (IntVal 1)) (Op2 (Var "x") Plus (Val (IntVal 1)))),
        P.parse matchP "match x with | [] -> 1 | x::xs -> 2"
          ~?= Right
            ( Match
                (Var "x")
                [ (ListPat [], Val (IntVal 1)),
                  (ConsPat (IdentifierPat "x") (IdentifierPat "xs"), Val (IntVal 2))
                ]
            )
      ]

