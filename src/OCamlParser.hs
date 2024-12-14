module OCamlParser where

import Control.Exception (PatternMatchFail)
import GHC.Base (many, undefined, (<|>))
import GHC.Generics (Par1)
import OCamlPrettyPrinter
import OCamlSyntax
import Parser as P
import Test.HUnit
import qualified Data.Char as Char
import System.IO (getContents)  -- For reading from standard input
import Test.QuickCheck as QC

-- Helper Parsers
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

parens :: Parser a -> Parser a
parens x = P.between (P.char '(') x (P.char ')')

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

constP :: String -> a -> Parser a
constP s x = x <$ wsP (P.string s)

stringP :: String -> Parser ()
stringP s = constP s ()

reserved :: [String]
reserved =
  [ "do",
    "else",
    "end",
    "false",
    "if",
    "in",
    "not",
    "then",
    "true",
    "fun",
    "fold",
    "transform",
    "begin",
    "match",
    "let"
  ]

--- Values
valueP :: Parser Value
valueP = intValP <|> boolValP <|> tupleValP <|> listValP <|> functionValP

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

boolValP :: Parser Value
boolValP = BoolVal <$> wsP parseBool
  where
    parseBool = P.choice [constP "true" True, constP "false" False]

tupleValP :: Parser Value
tupleValP = TupleVal <$> parens (wsP valueP `P.sepBy1` wsP (P.char ','))

listValP :: Parser Value
listValP = ListVal <$> brackets (wsP valueP `P.sepBy1` wsP (P.char ';'))

-- what do we do if it's fun x y -> x
functionValP :: Parser Value
functionValP = FunctionVal <$> (wsP (stringP "fun") *> wsP idP <* wsP (stringP "->")) <*> expP

-- >>> P.parse functionValP "fun x -> x + 1"
-- Right (FunctionVal "x" (Var "x"))

--- Identifier
idP :: Parser Identifier
idP = P.filter (`notElem` reserved) (wsP ((:) <$> (P.satisfy Char.isAlpha <|> P.char '_') <*>
  many (P.satisfy Char.isAlphaNum <|> P.char '_')))

--- Expressions
expP :: Parser Expression
expP = choice [
  varP,
  valP,
  op1P,
  op2P,
  listConstP,
  tupleConstP,
  functionConstP,
  ifP,
  letP,
  matchP
  ]

varP :: Parser Expression
varP = Var <$> idP

valP :: Parser Expression
valP = Val <$> (intValP <|> boolValP)

op1P :: Parser Expression
op1P = baseP <|> Op1 <$> uopP <*> op1P

baseP :: Parser Expression
baseP = valP            -- Parse literal values
  <|> varP              -- Parse variable identifiers
  <|> parens expP <|> listConstP

op2P :: Parser Expression
op2P = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Append)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = op1P `P.chainl1` opAtLevel (level Times)


opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>> P.parse op2P "3 + 4 * 2"
-- Right (Op2 (Val (IntVal 3)) Plus (Op2 (Val (IntVal 4)) Times (Val (IntVal 2))))

-- >>> P.parse op2P "3 * 4 + 2"
-- Right (Op2 (Op2 (Val (IntVal 3)) Times (Val (IntVal 4))) Plus (Val (IntVal 2)))

-- >>> P.parse expP "x + 3"
-- Right (Var "x")

-- >>> P.parse op2P "1 :: [2; 3]"
-- Right (Op2 (Val (IntVal 1)) Cons (ListConst [Val (IntVal 2),Val (IntVal 3)]))

listConstP :: Parser Expression
listConstP = ListConst <$> brackets (wsP expP `P.sepBy1` wsP (P.char ';'))

tupleConstP :: Parser Expression
tupleConstP = TupleConst <$> parens (wsP expP `P.sepBy1` wsP (P.char ','))

functionConstP :: Parser Expression
functionConstP = FunctionConst <$> (wsP (stringP "fun") *> idP) <*> expP

ifP :: Parser Expression
ifP = If <$> (wsP (stringP "if") *> expP) <*> (wsP (stringP "then") *> expP) <*> (wsP (stringP "else") *> expP)

matchP :: Parser Expression
matchP = undefined
  -- Match <$> ((wsP (stringP "match") *> idP <* wsP (stringP "with")))<*> many (patP) where
  -- patP :: Parser (Pattern, Expression)
  -- patP = undefined

letP :: Parser Expression
letP = Let <$> (wsP (stringP "let") *> idP <* wsP (P.char '=')) <*> expP <*> (wsP (stringP "in") *> expP)


--- Patterns
topLevelpatternP :: Parser Pattern
topLevelpatternP = wsP (P.char '|') *> choice [
  intConstPatP,
  boolConstPatP,
  wildcardPatP,
  identifierPatP,
  listPatP,
  tuplePatP,
  consPatP
  ]

otherPatternP :: Parser Pattern
otherPatternP = choice [
  intConstPatP,
  boolConstPatP,
  wildcardPatP,
  identifierPatP,
  listPatP,
  tuplePatP,
  consPatP
  ]

intConstPatP :: Parser Pattern
intConstPatP = IntConstPat <$> wsP P.int

boolConstPatP :: Parser Pattern
boolConstPatP = BoolConstPat <$> wsP parseBool
  where
    parseBool = P.choice [constP "true" True, constP "false" False]

identifierPatP :: Parser Pattern
identifierPatP = IdentifierPat <$> idP

listPatP :: Parser Pattern
listPatP = ListPat <$> brackets (wsP otherPatternP `P.sepBy1` wsP (P.char ';'))

consPatP :: Parser Pattern
consPatP = ConsPat <$> wsP otherPatternP <*> (wsP (stringP "::") *> wsP otherPatternP)

tuplePatP :: Parser Pattern
tuplePatP = TuplePat <$> parens (wsP otherPatternP `P.sepBy1` wsP (P.char ','))

wildcardPatP :: Parser Pattern
wildcardPatP = WildcardPat <$ wsP (P.char '_')

-- >>> P.parse topLevelpatternP "| x::xs::xss"
-- Right (IdentifierPat "x")

--- OPS

bopP :: Parser Bop
bopP = wsP (Plus <$ P.char '+' <|> Minus <$ P.char '-' <|> Times <$ P.char '*' <|> Divide <$ P.string "/" <|> Mod <$ P.string "mod"
        <|> Eq <$ P.string "=" <|> Ge <$ P.string ">=" <|> Gt <$ P.char '>' <|> Le <$ P.string "<=" <|> Lt <$ P.char '<' <|> Append <$ P.char '@' <|> Cons <$ P.string "::"
        <|> Or <$ P.string "||" <|> And <$ P.string "&&")

uopP :: Parser Uop
uopP = wsP (Neg <$ P.char '-' <|> Not <$ P.string "not")

--- statements
statementP :: Parser Statement
statementP = varDeclP 

varDeclP :: Parser Statement
varDeclP = VarDecl <$> wsP isRec <*> wsP idP <*> (wsP (P.char '=') *> wsP expP)
  where
    isRec = (True <$ wsP (P.string "let rec")) <|> (False <$ wsP (P.string "let"))

-- >>> P.parse varDeclP "let rec x = 4"
-- Right (VarDecl True "x" (Val (IntVal 4)))

blockP :: Parser Block
blockP = Block <$> many (wsP statementP)

--- top level

parseOcamlExp :: String -> Either ParseError Expression
parseOcamlExp = P.parse expP

parseOcamlStat :: String -> Either ParseError Statement
parseOcamlStat =  P.parse statementP

parseOcaml :: IO (Either ParseError Block)
parseOcaml = do
  input <- getContents
  let result = P.parse (const <$> blockP <*> P.eof) input
  return result
