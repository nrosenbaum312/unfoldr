module OCamlParser where

import Control.Exception (PatternMatchFail)
import GHC.Base (undefined)
import GHC.Generics (Par1)
import OCamlSyntax

--- quickCheck properties that ensures that parsing is the inverse of printing

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

--- Values
valueP :: Parser Value
valueP = intValP <|> boolValP <|> tupleValP <|> listValP <|> functionValP

intValP :: Parser Value
intValP = undefined

boolValP :: Parser Value
boolValP = undefined

tupleValP :: Parser Value
tupleValP = undefined

listValP :: Parser Value
listValP = undefined

functionValP :: Parser Value
functionValP = undefined

--- Identifier

idP :: Parser Identifier
idP = undefined

--- Expressions
expP :: Parser Expression
expP = undefined

varP :: Parser Expression
varP = undefined

valP :: Parser Expression
valP = undefined

op1P :: Parser Expression
op1P = undefined

op2P :: Parser Expression
op2P = undefined

listConstP :: Parser Expression
listConstP = undefined

tupleConstP :: Parser Expression
tupleConstP = undefined

functionConstP :: Parser Expression

fucntionConstP = undefined

ifP :: Parser Expression
ifP = undefined

matchP :: Parser Expression
matchP = undefined

letP :: Parser Expression
letP = undefined

--- Patterns
patternP :: Parser Pattern
patternP = undefined

intConstPatP :: Parser Pattern
intConstPatP = undefined

boolConstPatP :: Parser Pattern
boolConstPatP = undefined

identifierPatP :: Parser Pattern

indentifierPatP = undefined

listPatP :: Parser Pattern
listPatP = undefined

consPatP :: Parser Pattern
consPatP = undefined

tuplePatP :: Parser Pattern
tuplePatP = undefined

wildcardPatP :: Parser Pattern

wildCardPatP = undefined

--- OPS

bopP :: Parser Bop
bopP = undefined

uopP :: Parser Uop
uopP = undefined

--- statements
statementP :: Parser Statement
statementP = functionDeclP <|> varDeclP <|> emptyP

functionDeclP :: Parser Statement
functionDeclP = undefined

varDeclP :: Parser Statement
varDeclP = undefined

emptyP :: Parser Statement
emptyP = undefined

blockP :: Parser Block
blockP = undefined

--- top level

parseOcamlExp :: String -> Either P.ParseError Expression

parseLuExp = undefined

parseOcamlStat :: String -> Either P.ParseError Statement

parseLuStat = undefined

parseOcaml :: String -> IO (Either P.ParseError Block)
parseOcaml = undefined
