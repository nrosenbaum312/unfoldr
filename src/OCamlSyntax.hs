module OCamlSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

newtype Block = Block [Statement] -- s1 ... sn
  deriving (Eq, Show)

instance Semigroup Block where
  (<>) :: Block -> Block -> Block
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty :: Block
  mempty = Block []

type Identifier = String -- the name of a variable (including function variables/top-level decls)

data Value
  = IntVal Int -- -inf - inf
  | BoolVal Bool -- false, true
  | TupleVal [Value] -- (1, 2)
  | ListVal [Value] -- [1; 2]
  | FunctionVal Identifier Expression -- fun x -> x * 2 * y
  deriving (Eq, Show)

type Scope = Map Identifier Expression

data Expression
  = Var Identifier -- variables, in any scope
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | ListConst [Expression] -- list construction, like [1; 2; 3]
  | TupleConst [Expression] -- tuple construction, like (1, 2, 3)
  | FunctionConst Identifier Expression -- function construction, like fun x -> x * 2
  | If Expression Expression Expression -- If Expression then Expression else Expression
  | Match Expression [(Pattern, Expression)] -- match expression, like match x with | [] -> 1 | x::xs -> 2
  | Let Identifier Expression Expression -- let identifier = expression in expression
  | Apply Expression Expression -- function application, like f x
  deriving (Eq, Show)

data Pattern
  = IntConstPat Int -- match an integer constant
  | BoolConstPat Bool -- match a bool constant
  | IdentifierPat Identifier -- match an identifier
  | ListPat [Pattern] -- match a list of patterns (e.g., [x; y; z])
  | ConsPat Pattern Pattern -- match head-tail pattern (e.g., x::xs)
  | TuplePat [Pattern] -- match a tuple (e.g., (x, y))
  | WildcardPat -- match anything (underscore `_`)
  deriving (Show, Eq)

-- take a pattern, take a value, and figure out a substitution, then apply
-- the substitution.

-- substitution

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `/` :: Int -> Int -> Int   -- floor division
  | Mod -- `mod`  :: Int -> Int -> Int   -- modulo
  | Eq -- `=` :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  | Append -- `@` :: 'a List -> 'a List -> 'a List
  | Cons -- `::` :: 'a -> 'a List -> 'a List
  | Or -- `||`
  | And -- `&&`
  deriving (Eq, Show, Enum, Bounded)

data Statement
  = VarDecl Bool Identifier Expression
  | Empty -- ';'
  deriving (Eq, Show)

level :: Bop -> Int
level Times = 7
level Divide = 7
level Mod = 7
level Plus = 6
level Minus = 6
level Cons =  5
level Append = 5
level Eq = 4
level Gt = 4
level Ge = 4
level Lt = 4
level Le = 4
level Or = 3
level And = 3