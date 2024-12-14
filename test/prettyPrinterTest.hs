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
        pretty (IntConstPat 1) ~?= "1",
        pretty (BoolConstPat False) ~?= "false",
        pretty (IdentifierPat "X") ~?= "X",
        pretty (ListPat [IdentifierPat "X", IntConstPat 1]) ~?= "[X;1]",
        pretty (TuplePat [IdentifierPat "X", IntConstPat 1]) ~?= "(X,1)",
        pretty (ConsPat (IdentifierPat "x") (IdentifierPat "xs")) ~?= "x::xs",
        pretty WildcardPat ~?= "_"
      ]

-- >>> runTestTT test_print_patterns
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

test_print_statement :: Test
test_print_statement =
  "printing statements"
    ~: TestList
      [
        pretty (VarDecl True "x" (Op2 (Var "x") Plus (Val (IntVal 2)))) ~?= "let rec x = x + 2",
        pretty (VarDecl False "y" (If (Val (BoolVal True)) (Val (IntVal 20)) (TupleConst [Val (IntVal 10), Val (IntVal 20)])))
          ~?= "let y = if true then 20 else (10, 20)"
      ]

-- >>> runTestTT test_print_statement
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
