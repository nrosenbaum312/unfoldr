module OCamlStepper where

import Control.Monad (forM, unless, when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import OCamlParser qualified
import OCamlSyntax
import State
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

-- make an empty scope
makeScope :: OCamlSyntax.Scope
makeScope = Map.empty

-- add a binding to a scope
addBinding :: Identifier -> Value -> Scope -> Scope
addBinding = undefined

-- look up a variable in a scope
resolveId :: Identifier -> Scope -> Maybe Value
resolveId = undefined

evalExpression :: Expression -> State Scope Value
evalExpression = undefined

evalOp1 :: Uop -> Value -> Either String Value
evalOp1 = undefined

evalOp2 :: Bop -> Value -> Value -> Either String Value
evalOp2 = undefined

evalListConst :: [Expression] -> Value
evalListConst = undefined

evalTupleConst :: [Expression] -> Value
evalTupleConst = undefined

evalIf :: Expression -> Expression -> Expression -> Value
evalIf = undefined

evalMatch :: Expression -> [(Pattern, Expression)] -> Value
evalMatch = undefined

evalLet :: Identifier -> Expression -> Expression -> State Scope Value
evalLet = undefined

oneShotEvalBlock :: Block -> Either String Scope
oneShotEvalBlock = undefined

oneShotEvalStatement :: Statement -> State Scope ()
oneShotEvalStatement = undefined

data StepSize
  = Small
  | Medium
  | Large

stepExpression :: Expression -> State Scope Expression
stepExpression = undefined

{-

# Substitution Semantics

(fun x -> fun y -> x + y) 1 2
==> (fun y -> 1 + y) 2
==> 1 + 2
==> 3

Substitution recursively replaces variables with values, up until they're
shadowed by a same-name argument, let declaration, or pattern variable.

We would also have a scope to track top-level declarations, aka variables that
should not be immediately substituted but should instead be substituted lazily.

We also want to substitute recursive lets lazily, placing them into a Scope.

No local recursive lets please!

# How to Step Expressions

## Var
If we reach a Var expression, we're encountering a lazily-substituted value
and we should do the substitution only once.

## Value
We don't need to step values; they're already values! But replacing by a Value
is a "small" step.

## Op1
We first evaluate the argument. If the argument is a larger step, then we just
return the op unevaluated with the value. Else, we evaluate the op as a "large"
step.

## Op2
Similar to Op1, but starting with the first argument and then the second. This
is a "large" step.

## ListConst
Evaluate Evaluates directly to a value as a small step.

## TupleConst
Evaluates directly to a value as a small step.

## FunctionConst
Evaluates directly to a value as a small step.

## If
Step the predicate to a value; then replace the if with the right branch as a
"large" step.

## Match
Step the matched expression to a value; then replace the match with the right
branch, substituted with the values, in one "large" step.

## Let
Step the expression to a value; substitute the variable for the value in the
body of the let statement.

## Apply
Step the expression to a function value; then step the arguments to values, in
order.
-}