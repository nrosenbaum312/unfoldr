module Main where

import Control.Monad (when)
import Parser
import OCamlParser
import Data.List
import OCamlStepper (stepBlock, makeScope, stepExpToValue, stepExpToValueWithScope)
import State
import OCamlSyntax

readUntilThreeEmptyLines :: IO [String]
readUntilThreeEmptyLines = go [] 0
  where
    go acc emptyCount = do
      line <- getLine
      if line == ""
        then
          if emptyCount + 1 == 2
            then return (reverse acc)
            else go acc (emptyCount + 1)
        else go (line : acc) 0

whileM :: IO Bool -> IO () -> IO ()
whileM cond action = do
  c <- cond
  when c $ action >> whileM cond action

stepWhileNotValue :: Scope -> Expression -> IO () -> IO ()
stepWhileNotValue s = undefined

main :: IO ()
main = do
  putStrLn "Please enter your top-level lets, followed by two newlines:"
  topLevelLets <- readUntilThreeEmptyLines
  let parsed = parse blockP (intercalate "\n" topLevelLets)
  case parsed of
    Left s -> putStrLn s
    Right block -> do
      putStrLn "Now, please enter your expression:"
      expression <- getLine
      let parsedExp = parse expP expression
      case parsedExp of
        Left s -> putStrLn s
        Right exp -> do
          putStrLn "Result:"
          let (_, scope) = State.runState (stepBlock block) makeScope
          let res = stepExpToValueWithScope exp scope
          case res of
            Left s -> putStrLn s
            Right val -> print val
