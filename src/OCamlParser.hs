module OCamlParser where

import Control.Exception (PatternMatchFail)
import GHC.Base (undefined, (<|>))
import GHC.Generics (Par1)
import OCamlPrettyPrinter
import OCamlSyntax
import Parser

--- quickCheck properties that ensures that parsing is the inverse of printing

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP (pretty s) == Right s

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
functionConstP = undefined

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
identifierPatP = undefined

listPatP :: Parser Pattern
listPatP = undefined

consPatP :: Parser Pattern
consPatP = undefined

tuplePatP :: Parser Pattern
tuplePatP = undefined

wildcardPatP :: Parser Pattern
wildcardPatP = undefined

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

parseOCamlExp :: String -> Either ParseError Expression
parseOCamlExp = undefined

parseOCamlStat :: String -> Either ParseError Statement
parseOCamlStat = undefined

parseOcaml :: String -> IO (Either ParseError Block)
parseOcaml = undefined
