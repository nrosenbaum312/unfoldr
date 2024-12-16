module Main where

import Control.Monad (when)

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

main :: IO ()
main = do
  putStrLn "Please enter your top-level lets, followed by two newlines:"
  topLevelLets <- readUntilThreeEmptyLines
  putStrLn "Now, please enter your expression:"
  expression <- getLine
  putStrLn "Result:"
