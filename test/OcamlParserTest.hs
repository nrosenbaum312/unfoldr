module OcamlParserTest where
import OCamlParser
import Control.Exception (PatternMatchFail)
import GHC.Base (many, undefined, (<|>))
import GHC.Generics (Par1)
import OCamlPrettyPrinter as PP
import OCamlSyntax
    ( Bop(Plus, Or, Times),
      Expression(Val, ListConst, TupleConst, FunctionConst, If, Let,
                 Match, Op1, Var, Op2),
      Pattern(IdentifierPat, BoolConstPat, WildcardPat, ListPat,
              TuplePat, IntConstPat, ConsPat),
      Statement,
      Uop(Not, Neg),
      Value(..) )
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
        P.parse listValP "[1; 2; 3]" ~?= Right (ListVal [IntVal 1, IntVal 2, IntVal 3])
      ]

-- >>> runTestTT test_value
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

test_functionVal :: Test
test_functionVal =
  "parsing functionVals"
    ~: TestList
      [ P.parse functionValP "fun x -> x + 1" ~?= Right (FunctionVal "x" (Op2 (Var "x") Plus (Val (IntVal 1)))),
      P.parse functionValP "fun x y z -> x + 1" ~?=  Right (FunctionVal "x" (FunctionConst "y" (FunctionConst "z" (Op2 (Var "x") Plus (Val (IntVal 1))))))]


-- >>> runTestTT test_functionVal
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

test_expression :: Test
test_expression =
  "parsing expressions"
    ~: TestList
      [ P.parse expP "x" ~?= Right (Var "x"),
        P.parse expP "42" ~?= Right (Val (IntVal 42)),
        P.parse expP "-x" ~?= Right (Op1 Neg (Var "x")),
        P.parse expP "x + 1" ~?= Right (Op2 (Var "x") Plus (Val (IntVal 1))),
        P.parse expP "[1; 2; 3]" ~?= Right (ListConst [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)]),
        P.parse expP "[(1,1); (2,2)]" ~?= Right (ListConst [TupleConst [Val (IntVal 1), Val (IntVal 1)], TupleConst [Val (IntVal 2), Val (IntVal 2)]]),
        P.parse expP "(1, x)" ~?= Right (TupleConst [Val (IntVal 1), Var "x"]),
        P.parse expP "fun x y -> x + y"
          ~?= Right (FunctionConst "x" (FunctionConst "y" (Op2 (Var "x") Plus (Var "y")))),
        P.parse expP "fun x -> fun y -> x + y"
          ~?= Right (FunctionConst "x" (FunctionConst "y" (Op2 (Var "x") Plus (Var "y")))),
        P.parse expP "if true then 1 else 0"
          ~?= Right (If (Val (BoolVal True)) (Val (IntVal 1)) (Val (IntVal 0))),
        P.parse expP "let x = 1 in x + 1"
          ~?= Right (Let "x" (Val (IntVal 1)) (Op2 (Var "x") Plus (Val (IntVal 1)))),
        P.parse expP "begin match x with | [] -> 1 | x::xs -> 2 end"
          ~?= Right
            ( Match
                (Var "x")
                [ (ListPat [], Val (IntVal 1)),
                  (ConsPat (IdentifierPat "x") (IdentifierPat "xs"), Val (IntVal 2))
                ]
            )
      ]

-- >>> runTestTT test_expression
-- Counts {cases = 12, tried = 12, errors = 0, failures = 0}

test_op_expression :: Test
test_op_expression =
  "parsing op expressions"
    ~: TestList [
      P.parse expP "-(-x)" ~?= Right (Op1 Neg (Op1 Neg (Var "x"))),
      P.parse expP "(-x)" ~?= Right (Op1 Neg (Var "x")),
      P.parse expP "not a"  ~?= Right (Op1 Not (Var "a")),
      P.parse expP "-(13)" ~?= Right (Op1 Neg (Val (IntVal 13))),
      P.parse expP "-13" ~?= Right (Val (IntVal (-13))),
      P.parse expP "not (x || y)" ~?= Right (Op1 Not (Op2 (Var "x") Or (Var "y")))
    ]

-- >>> runTestTT test_op_expression
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

test_bop_expression :: Test
test_bop_expression =
  "parsing bops"
  ~: TestList [
    P.parse expP "1 + 2 * 3" ~?=  Right (Op2 (Val (IntVal 1)) Plus (Op2 (Val (IntVal 2)) Times (Val (IntVal 3)))),
    P.parse expP "(1 + 2) * 3" ~?=  Right (Op2 (Op2 (Val (IntVal 1)) Plus (Val (IntVal 2))) Times (Val (IntVal 3))),
    P.parse expP "((1 + 2) * 3)" ~?=  Right (Op2 (Op2 (Val (IntVal 1)) Plus (Val (IntVal 2))) Times (Val (IntVal 3))),
    P.parse expP "1 * (2 + 3)" ~?= Right (Op2 (Val (IntVal 1)) Times (Op2 (Val (IntVal 2)) Plus (Val (IntVal 3)))),
    P.parse expP "(1 * (2 + 3))" ~?= Right (Op2 (Val (IntVal 1)) Times (Op2 (Val (IntVal 2)) Plus (Val (IntVal 3))))
  ]

-- >>> runTestTT test_bop_expression
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_pattern :: Test
test_pattern =
  "parsing patterns"
  ~: TestList
    [
      P.parse topLevelPatternP "| 1" ~?= Right (IntConstPat 1),
      P.parse topLevelPatternP "| true" ~?= Right (BoolConstPat True),
      P.parse topLevelPatternP "| _" ~?= Right WildcardPat,
      P.parse topLevelPatternP "| [1; (x, y)]" ~?= Right (ListPat [IntConstPat 1,TuplePat [IdentifierPat "x",IdentifierPat "y"]]),
      P.parse topLevelPatternP "| (x, 1)" ~?= Right (TuplePat [IdentifierPat "x", IntConstPat 1]),
      P.parse topLevelPatternP "| x" ~?= Right (IdentifierPat "x"),
      P.parse topLevelPatternP "| x::xs" ~?= Right (ConsPat (IdentifierPat "x") (IdentifierPat "xs")),
      P.parse topLevelPatternP "| x::xs::xss" ~?= Right (ConsPat (ConsPat (IdentifierPat "x") (IdentifierPat "xs")) (IdentifierPat "xss"))
    ]

-- >>> runTestTT test_pattern
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}


