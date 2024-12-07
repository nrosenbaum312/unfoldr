import Test.HUnit
import Test.QuickCheck
import OCamlPrettyPrinter
import OCamlSyntax

test_print_vals :: Test
test_print_vals =
    "printing values"
      ~: TestList
        [
            pretty (IntVal 1) ~?= "1",
            pretty (BoolVal True) ~?= "true",
            pretty (TupleVal [IntVal 1, IntVal 2, BoolVal True]) ~?= "(1, 2, true)",
            pretty (ListVal [IntVal 1, IntVal 2, BoolVal True]) ~?= "[1, 2, true]",
            pretty (FunctionVal "x" (Op1 Not (Val (BoolVal False)))) ~?= "fun x -> not false"
        ]

-- >>> runTestTT test_print_vals
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_print_expressions :: Test
test_print_expressions = 
  "printing expressions"
    ~: TestList
      [
        pretty (Var "x") ~?= "x",
        pretty (Val (IntVal 1)) ~?= "1",
        pretty (Op1 Neg (Val (IntVal 1))) ~?= "-1",
        pretty (Op1 Not (Val (BoolVal True))) ~?= "not true",
        pretty (Op2 (Val (IntVal 1)) Plus (Val (IntVal 10))) ~?= "1 + 10",
        pretty (ListConst [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)]) ~?= "[1; 2; 3]",
        pretty (TupleConst [Val (IntVal 1), Val (IntVal 2)]) ~?= "(1, 2)",
        pretty (FunctionConst "x" (Op1 Neg (Val (IntVal 2)))) ~?= "fun x -> -2",
        pretty (FunctionConst "x" (Op1 Neg (Var "x"))) ~?= "fun x -> -x",
        pretty (If (Val (BoolVal True)) (TupleConst [Val $ IntVal 1, Val $ IntVal 2]) (TupleConst [Val $ IntVal 2, Val $ IntVal 1]))
          ~?= "if true then (1, 2) else (2, 1)",
        pretty (Let "x" (Val (IntVal 1)) (Op1 Not (Var "x"))) ~?= "let x = 1 in\nnot x"
      ]

-- >>> runTestTT test_print_expressions
-- Counts {cases = 11, tried = 11, errors = 0, failures = 0}

test_print_patterns :: Test
test_print_patterns =
  "printing patterns"
    ~: TestList
      [
        pretty (IntConstPat 1) ~?= "1"
      ]

test_print_statement :: Test
test_print_statement =
  "printing statements"
    ~: TestList
      [
        
      ]
