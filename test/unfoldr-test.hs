import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck qualified as QC

-- import OCamlParserTest as PT
-- import PrettyPrinterTest as PPT

-- test_all :: IO Counts
-- test_all = runTestTT $ TestList [test_value, test_functionVal, test_expression, test_op_expression, test_bop_expression, test_pattern]

main :: IO ()
main = do 
    putStrLn "OCaml Parser Tests"
    putStrLn "Pretty Printer Tests"
    putStrLn "QuickCheck Properties"