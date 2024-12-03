import Test.HUnit
import Test.QuickCheck
import OCamlPrettyPrinter
import OCamlSyntax

test_print_vals :: Test
test_print_vals =
    "printing values"
      ~: TestList
        [
            pretty (IntVal 1) ~?= "1"
        ]

test_print_expressions :: Test
test_print_expressions = 
  "printing expressions"
    ~: TestList
      [
        pretty (Var "x") ~?= "x"
      ]

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