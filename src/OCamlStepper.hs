module OCamlStepper where

import Control.Monad (foldM, forM, mapM, unless, when, zipWithM)
import Data.Bifunctor (second)
import Data.Either
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import OCamlParser qualified
import OCamlSyntax
import OCamlTypes
import State
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

-- make an empty scope
makeScope :: Scope
makeScope = Map.empty

-- add a binding to a scope
addBinding :: Identifier -> Expression -> Scope -> Scope
addBinding = Map.insert

-- look up a variable in a scope
resolveId :: Identifier -> State Scope (Maybe Expression)
resolveId id = do
  scope <- State.get
  case Map.lookup id scope of
    Just value -> return $ Just value
    Nothing -> return Nothing

-- Substitutes an id for its bound expression in the sub-expression
substitute :: Identifier -> Expression -> Expression -> Expression
substitute id valExpression (Var vid) = if vid == id then valExpression else Var vid
substitute id valExpression (Val value) = Val value
substitute id valExpression (Op1 uop e) = Op1 uop (substitute id valExpression e)
substitute id valExpression (Op2 e1 bop e2) = Op2 (substitute id valExpression e1) bop (substitute id valExpression e2)
substitute id valExpression (ListConst es) = ListConst (map (substitute id valExpression) es)
substitute id valExpression (TupleConst es) = TupleConst (map (substitute id valExpression) es)
substitute id valExpression ex@(FunctionConst a e) =
  if a == id then ex else FunctionConst a (substitute id valExpression e) -- shadowing! only substitute if we're not redefining x
substitute id valExpression (If guard t e) = If (substitute id valExpression guard) (substitute id valExpression t) (substitute id valExpression e)
substitute id valExpression (Match v patterns) =
  Match (substitute id valExpression v) (map substituteIfNotShadowed patterns)
  where
    substituteIfNotShadowed (pat, exp) =
      if id `elem` extractIdsFromPattern [pat]
        then (pat, exp)
        else (pat, substitute id valExpression exp)
substitute id valExpression ex@(Let i ve ie) =
  if i == id then ex else Let i (substitute id valExpression ve) (substitute id valExpression ie)
substitute id valExpression (Apply fn arg) = Apply (substitute id valExpression fn) (substitute id valExpression arg)

extractIdsFromPattern :: [Pattern] -> [Identifier]
extractIdsFromPattern [] = []
extractIdsFromPattern (pat : pats) =
  ( case pat of
      IdentifierPat id -> [id]
      ListPat subpats -> extractIdsFromPattern subpats
      ConsPat p1 p2 -> extractIdsFromPattern [p1, p2]
      TuplePat subpats -> extractIdsFromPattern subpats
      _ -> []
  )
    ++ extractIdsFromPattern pats

-- >>> substitute "x" (Val (IntVal 2)) (Var "x")
-- Val (IntVal 2)
-- >>> substitute "x" (Val (IntVal 2)) (Val (IntVal 3))
-- Val (IntVal 3)
-- >>> substitute "x" (Val (IntVal 2)) (Op1 Neg (Var "x"))
-- Op1 Neg (Val (IntVal 2))
-- >>> substitute "x" (Val (IntVal 2)) (Op2 (Var "y") Times (Var "x"))
-- Op2 (Var "y") Times (Val (IntVal 2))
-- >>> substitute "x" (Val (IntVal 2)) (ListConst [(Var "x"), (Val $ IntVal 2)])
-- ListConst [Val (IntVal 2),Val (IntVal 2)]
-- >>> substitute "x" (Val (IntVal 2)) (TupleConst [(Var "x"), (Val $ IntVal 2)])
-- TupleConst [Val (IntVal 2),Val (IntVal 2)]
-- >>> substitute "x" (Val (IntVal 2)) (FunctionConst "f" (Op1 Neg (Var "x")))
-- FunctionConst "f" (Op1 Neg (Val (IntVal 2)))
-- >>> substitute "x" (Val (IntVal 2)) (FunctionConst "x" (Op1 Neg (Var "x")))
-- FunctionConst "x" (Op1 Neg (Var "x"))
-- >>> substitute "x" (Val (IntVal 2)) (Let "x" (Val (BoolVal True)) (Var "x"))
-- Let "x" (Val (BoolVal True)) (Var "x")
-- >>> substitute "x" (Val (IntVal 2)) (Match (Var "y") [(IdentifierPat "x", Op1 Neg (Var "x")), (IntConstPat 2, Op1 Neg (Var "x"))])
-- Match (Var "y") [(IdentifierPat "x",Op1 Neg (Var "x")),(IntConstPat 2,Op1 Neg (Val (IntVal 2)))]

data Step e v
  = Large e
  | Small e
  | Final v
  deriving (Eq, Show)

type ExpressionStep = Step Expression Value

stepExpToValue :: Expression -> Either String Value
stepExpToValue (Val v) = Right v
stepExpToValue e =
  let s' = largeStepExp e
   in case evalState s' makeScope of
        Left s -> Left s
        Right e' -> stepExpToValue e'

stepGoodExpToExp :: Expression -> Expression
stepGoodExpToExp e =
  let s' = largeStepExp e
   in case evalState s' makeScope of
        Left s -> error "Undefined expression generated in test!"
        Right e' -> e'

stepExpN :: Int -> Expression -> State Scope (Either String Expression)
stepExpN 0 e = return $ Right e
stepExpN i e = do
  e' <- largeStepExp e
  case e' of
    Left s -> return $ Left s
    Right e'' -> stepExpN (i - 1) e''

-- >>> State.runState (stepExpN 3 (Op1 Neg (Var "x"))) (addBinding "x" (Val (IntVal 3)) makeScope)
-- (Right (Val (IntVal (-3))),fromList [("x",Val (IntVal 3))])

-- >>> State.runState (stepExpN 3 (Op2 (Var "x") Plus (Var "x"))) (addBinding "x" (Val (IntVal 3)) makeScope)
-- (Right (Val (IntVal 6)),fromList [("x",Val (IntVal 3))])

-- Steps the given expression until a large step is taken.
largeStepExp :: Expression -> State Scope (Either String Expression)
largeStepExp e = do
  e' <- stepExp e
  case e' of
    Right (Small e'') -> largeStepExp e''
    Right (Large e'') -> return $ Right e''
    Right (Final v) -> return $ Right (Val v)
    Left s -> return $ Left s

-- Steps the given expression.
stepExp :: Expression -> State Scope (Either String ExpressionStep)
stepExp (Var id) = stepVar id
stepExp (Val val) = return $ Right $ Final val
stepExp (Op1 uop exp) = stepUop uop exp
stepExp (Op2 e1 bop e2) = stepBop e1 bop e2
stepExp (ListConst es) = stepListConst es
stepExp (TupleConst es) = stepListConst es
stepExp (FunctionConst id f) = return $ Right $ Small $ Val (FunctionVal id f)
stepExp (If g i e) = stepIf g i e
stepExp (Match val pats) = stepMatch val pats
stepExp (Let id val ine) = stepLet id val ine
stepExp (Apply fn arg) = stepApply fn arg

-- >>> State.runState (stepExpN 1 (If (Op2 (Op1 Neg (Val (IntVal 1))) Eq (Val (IntVal (-1)))) (Val (IntVal 10)) (Val (IntVal 20)))) makeScope
-- (Right (If (Op2 (Val (IntVal (-1))) Eq (Val (IntVal (-1)))) (Val (IntVal 10)) (Val (IntVal 20))),fromList [])

-- >>> State.runState (stepExpN 2 (If (Op2 (Op1 Neg (Val (IntVal 1))) Eq (Val (IntVal (-1)))) (Val (IntVal 10)) (Val (IntVal 20)))) makeScope
-- (Right (If (Val (BoolVal True)) (Val (IntVal 10)) (Val (IntVal 20))),fromList [])

-- >>> State.runState (stepExpN 3 (If (Op2 (Op1 Neg (Val (IntVal 1))) Eq (Val (IntVal (-1)))) (Val (IntVal 10)) (Val (IntVal 20)))) makeScope
-- (Right (Val (IntVal 10)),fromList [])

-- >>> stepExpToValue (If (Op2 (Op1 Neg (Val (IntVal 1))) Eq (Val (IntVal (-1)))) (Val (IntVal 10)) (Val (IntVal 20)))
-- Right (IntVal 10)

-- Evaluates a variable expression by attempting to look up the variable in the
-- scope.
stepVar :: Identifier -> State Scope (Either String ExpressionStep)
stepVar id = do
  maybeExp <- resolveId id
  case maybeExp of
    Nothing -> return $ Left ("Variable " ++ id ++ " not found! Make sure you've defined it.")
    Just e -> return $ Right (Large e)

stepUop :: Uop -> Expression -> State Scope (Either String ExpressionStep)
stepUop uop (Val v) = return $ do
  v' <- evalUop uop v
  return $ Final v'
stepUop uop e = do
  e' <- stepExp e
  case e' of
    Right (Small e'') -> stepUop uop e''
    Right (Large e'') -> return $ Right $ Large (Op1 uop e'')
    Right (Final v) -> return $ Right $ Final v
    _ -> return e'

evalUop :: Uop -> Value -> Either String Value
evalUop Neg (IntVal i) = Right $ IntVal (-i)
evalUop Not (BoolVal b) = Right $ BoolVal (not b)
evalUop u v = Left $ "Type error: can't apply operator " ++ show u ++ " to a value of type " ++ showType v ++ "!"

stepBop :: Expression -> Bop -> Expression -> State Scope (Either String ExpressionStep)
stepBop (Val v1) bop (Val v2) = return $ do
  res <- evalBop v1 bop v2
  return $ Final res
stepBop (Val v1) bop r = do
  r' <- stepExp r
  case r' of
    Right (Small r'') -> stepBop (Val v1) bop r''
    Right (Large r'') -> return $ Right $ Large (Op2 (Val v1) bop r'')
    Right (Final v) -> return $ Right $ Large (Op2 (Val v1) bop (Val v))
    _ -> return r'
stepBop l bop r = do
  l' <- stepExp l
  case l' of
    Right (Small l'') -> stepBop l'' bop r
    Right (Large l'') -> return $ Right $ Large (Op2 l'' bop r)
    Right (Final v) -> return $ Right $ Large (Op2 (Val v) bop r)
    _ -> return l'

evalBop :: Value -> Bop -> Value -> Either String Value
evalBop (IntVal i1) Plus (IntVal i2) = Right $ IntVal (i1 + i2)
evalBop (IntVal i1) Minus (IntVal i2) = Right $ IntVal (i1 - i2)
evalBop (IntVal i1) Times (IntVal i2) = Right $ IntVal (i1 * i2)
evalBop (IntVal i1) Divide (IntVal i2) = Right $ IntVal (i1 `div` i2)
evalBop (IntVal i1) Mod (IntVal i2) = Right $ IntVal (i1 `mod` i2)
evalBop (IntVal i1) Eq (IntVal i2) = Right $ BoolVal (i1 == i2)
evalBop (IntVal i1) Gt (IntVal i2) = Right $ BoolVal (i1 > i2)
evalBop (IntVal i1) Ge (IntVal i2) = Right $ BoolVal (i1 >= i2)
evalBop (IntVal i1) Lt (IntVal i2) = Right $ BoolVal (i1 < i2)
evalBop (IntVal i1) Le (IntVal i2) = Right $ BoolVal (i1 <= i2)
evalBop (BoolVal b1) Or (BoolVal b2) = Right $ BoolVal (b1 || b2)
evalBop (BoolVal b1) And (BoolVal b2) = Right $ BoolVal (b1 && b2)
evalBop (ListVal l1) Append (ListVal l2) =
  let (l1Type, l2Type) = (typeofl l1, typeofl l2)
   in if l1Type == l2Type
        then Right $ ListVal (l1 ++ l2)
        else Left $ "Type error: can't append a list of type " ++ show l1Type ++ "to a list of type " ++ show l2Type
evalBop v Cons (ListVal l) =
  let ltype = typeofl l
   in if typeof v == ltype
        then
          Right (ListVal (v : l))
        else
          Left $ "Type error: can't cons an element of type " ++ show (typeof v) ++ " into a list of type " ++ show l
evalBop l b r = Left $ "Type error: can't perform operation " ++ show b ++ " on values of type " ++ show (typeof l) ++ " and " ++ show (typeof r)

-- >>> State.runState (stepListConst [Op1 Neg (Val (IntVal 1))]) makeScope
-- (Right (Final (ListVal [IntVal (-1)])),fromList [])

-- >>> State.runState (stepListConst [(Val (IntVal 2)), Op1 Neg (Val (IntVal 1)), Op1 Neg (Val (IntVal 1))]) makeScope
-- (Right (Large (ListConst [Val (IntVal 2),Val (IntVal (-1)),Op1 Neg (Val (IntVal 1))])),fromList [])

stepListConst :: [Expression] -> State Scope (Either String ExpressionStep)
stepListConst [] = return $ Right $ Small $ Val $ ListVal []
stepListConst l = do
  es <- valueify l
  case es of
    Right (Small r') -> return $ Right $ Small $ ListConst r'
    Right (Large r') -> return $ Right $ Large $ ListConst r'
    Right (Final vs) -> return $ Right $ Final $ ListVal vs
    Left s -> return $ Left s

valueify :: [Expression] -> State Scope (Either String (Step [Expression] [Value]))
valueify [] = return $ Right $ Small []
valueify (Val v : xs) = do
  rest <- valueify xs
  case rest of
    Right (Small r') -> valueify (Val v : r')
    Right (Large r') -> return $ Right $ Large (Val v : r')
    Right (Final vs) -> return $ Right $ Final (v : vs)
    Left s -> return $ Left s
valueify [x] = do
  x' <- stepExp x
  case x' of
    Right (Small x'') -> valueify [x'']
    Right (Large x'') -> return $ Right $ Large [x'']
    Right (Final v) -> return $ Right $ Final [v]
    Left s -> return $ Left s
valueify (x : xs) = do
  x' <- stepExp x
  case x' of
    Right (Small x'') -> valueify (x'' : xs)
    Right (Large x'') -> return $ Right $ Large (x'' : xs)
    Right (Final v) -> return $ Right $ Large $ Val v : xs
    Left s -> return $ Left s

stepIf :: Expression -> Expression -> Expression -> State Scope (Either String ExpressionStep)
stepIf (Val v) ife elsee = return $ do
  case v of
    BoolVal True -> Right $ Large ife
    BoolVal False -> Right $ Large elsee
    _ -> Left "Type error: the expression in an if statement has to be a boolean!"
stepIf guard ife elsee = do
  g' <- stepExp guard
  case g' of
    Right (Small g'') -> stepIf g'' ife elsee
    Right (Large g'') -> return $ Right $ Large (If g'' ife elsee)
    Right (Final v) -> return $ Right $ Large (If (Val v) ife elsee)
    _ -> return g'

stepLet :: Identifier -> Expression -> Expression -> State Scope (Either String ExpressionStep)
stepLet id (Val v) ine = return $ Right $ Large $ substitute id (Val v) ine
stepLet id val ine = do
  v' <- stepExp val
  case v' of
    Right (Small v'') -> stepLet id v'' ine
    Right (Large v'') -> return $ Right $ Large (Let id v'' ine)
    Right (Final v) -> return $ Right $ Large (Let id (Val v) ine)
    _ -> return v'

stepApply :: Expression -> Expression -> State Scope (Either String ExpressionStep)
stepApply (Val (FunctionVal arg body)) argVal = return $ Right $ Large $ substitute arg argVal body
stepApply (Val _) _ = return $ Left "Type error: only functions can be applied!"
stepApply fn argVal = do
  f' <- stepExp fn
  case f' of
    Right (Small f'') -> stepApply f'' argVal
    Right (Large f'') -> return $ Right $ Large (Apply f'' argVal)
    Right (Final v) -> return $ Right $ Large (Apply (Val v) argVal)
    _ -> return f'

match :: Value -> Pattern -> Maybe [(Identifier, Expression)]
match v (IdentifierPat id) = Just [(id, Val v)]
match v WildcardPat = Just []
match (IntVal i) (IntConstPat pi) = if i == pi then Just [] else Nothing
match (BoolVal b) (BoolConstPat bi) = if b == bi then Just [] else Nothing
match (TupleVal t) (TuplePat pats) = do
  bindings <- zipWithM match t pats
  return (concat bindings)
match (ListVal l) (ListPat pats) = do
  bindings <- zipWithM match l pats
  return (concat bindings)
match (ListVal (p : ps)) (ConsPat elp lp) = do
  elBindings <- match p elp
  lBindings <- match (ListVal ps) lp
  return $ elBindings ++ lBindings
match _ _ = Nothing

stepMatch :: Expression -> [(Pattern, Expression)] -> State Scope (Either String ExpressionStep)
stepMatch e@(Val v) ((p, arm) : ps) =
  case match v p of
    Nothing -> return $ Left "Failed to find a matching pattern!"
    Just bindings -> return $ Right $ Large $ foldl (\ine (id, e) -> substitute id e ine) arm bindings
stepMatch e arms = do
  e' <- stepExp e
  case e' of
    Right (Small e'') -> stepMatch e'' arms
    Right (Large e'') -> return $ Right $ Large (Match e'' arms)
    Right (Final v) -> return $ Right $ Large (Match (Val v) arms)
    _ -> return e'

prop_steppingDoesNotModifyValue :: Expression -> Property
prop_steppingDoesNotModifyValue e =
  isRight (stepExpToValue e) QC.==> stepExpToValue (stepGoodExpToExp e) == stepExpToValue e

test_stepExpressionToValue :: Test
test_stepExpressionToValue =
  "stepping expressions"
    ~: TestList
      [ stepExpToValue (Let "f" (FunctionConst "x" (Op2 (Var "x") Plus (Val (IntVal 2)))) (Apply (Var "f") (Val (IntVal 2)))) ~?= Right (IntVal 4)
      ]

-- >>> State.evalState (stepLet "f" (FunctionConst "x" (Op2 (Var "x") Plus (Val (IntVal 2)))) (Apply (Var "f") (Val (IntVal 2)))) makeScope
-- Right (Large (Apply (Val (FunctionVal "x" (Op2 (Var "x") Plus (Val (IntVal 2))))) (Val (IntVal 2))))

-- >>> stepExpToValue (Let "f" (FunctionConst "x" (Op2 (Var "x") Plus (Val (IntVal 2)))) (Apply (Var "f") (Val (IntVal 2))))
-- Right (IntVal 4)

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

Let "x" (Val (IntVal 3)) (Op2 (Var "x") Times (Val (IntVal 2)))

smallStepLet id valExpression inExpression =
  substitute id valExpression inExpression

(Op2 (Val (IntVal 3)) Times (Val (IntVal 2)))

## Apply
Step the expression to a function value; then step the arguments to values, in
order.
-}
