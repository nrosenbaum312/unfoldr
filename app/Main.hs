module Main where

import Control.Monad (when)
import Parser
import OCamlParser
import Data.List
import OCamlStepper (stepBlock, makeScope, stepExpToValue, stepExpToValueWithScope, largeStepExp)
import State
import OCamlSyntax
import OCamlPrettyPrinter
import GHC.IO.Exception (IOException)
import Control.Exception (catch)
import System.IO (hGetLine, stdin)

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

whileM :: IO Bool -> IO ()
whileM action = do
  c <- action
  when c $ whileM action

printSteps :: Expression -> Scope -> Int -> IO ()
printSteps e@(Val (FunctionVal id body)) s i = doStep e s i
printSteps (Val v) s i = do
  putStr "==> "
  putStrLn $ pretty v
printSteps e s i = doStep e s i

doStep :: Expression -> Scope -> Int -> IO ()
doStep e s i =
  let s' = largeStepExp e
   in case evalState s' s of
      Left s -> putStrLn s
      Right e' -> do
        putStr $ " " ++ show i ++ ") "
        putStrLn $ pretty e'
        printSteps e' s (i + 1)

safeGetLine :: IO (Maybe String)
safeGetLine = catch (Just <$> getLine) handleEOF
  where
    handleEOF :: IOException -> IO (Maybe String)
    handleEOF _ = return Nothing

transform = "let rec transform = fun f l -> begin match l with | [] -> [] | (x::xs) -> (f x) :: transform f xs end"
fold = "let rec fold = fun combine b l -> begin match l with | [] -> b | (x::xs) -> combine x (fold combine b xs) end"

main :: IO ()
main = do
  putStrLn "Enter your top-level lets, followed by two newlines:"
  topLevelLets <- readUntilThreeEmptyLines
  let parsed = parse blockP (intercalate "\n" (transform:fold:topLevelLets))
  case parsed of
    Left s -> putStrLn s
    Right block ->
      whileM (
        do
          putStrLn "Enter an expression:"
          maybeExpression <- safeGetLine
          case maybeExpression of
            Nothing -> do
              putStrLn "Bye!"
              return False
            Just expression -> do
              let parsedExp = parse expP expression
              case parsedExp of
                Left s -> do
                  putStrLn s
                  return False
                Right exp -> do
                  putStrLn ""
                  let (_, scope) = State.runState (stepBlock block) makeScope
                  putStr " 0) "
                  putStrLn $ pretty exp
                  printSteps exp scope 1
                  putStrLn ""
                  return True
      )
