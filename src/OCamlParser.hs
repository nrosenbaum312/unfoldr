module OCamlParser where

import Control.Exception (PatternMatchFail)
import GHC.Base (many, undefined, (<|>))
import GHC.Generics (Par1)
import OCamlPrettyPrinter
import OCamlSyntax
import Parser as P
import Test.HUnit
import qualified Data.Char as Char

--- quickCheck properties that ensures that parsing is the inverse of printing

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP (pretty s) == Right s

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

test_value :: Test
test_value =
  "parsing values"
    ~: TestList
      [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        P.parse tupleValP "(1, 2)" ~?= Right (TupleVal [IntVal 1, IntVal 2]),
        P.parse listValP "[1; 2; 3]" ~?= Right (ListVal [IntVal 1, IntVal 2, IntVal 3]),
        P.parse functionValP "fun x -> x + 1" ~?= Right (FunctionVal "x" (Op2 (Var "x") Plus (Val (IntVal 1))))
      ]

-- >>> runTestTT test_value
-- Counts {cases = 5, tried = 5, errors = 1, failures = 1}

-- >>> P.parse functionValP "fun x -> x + 1"
-- Prelude.undefined

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

-- >>> P.parse op2P "1 :: [2; 3]"
-- Right (Val (IntVal 1))
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

test_expression :: Test
test_expression =
  "parsing expressions"
    ~: TestList
      [ P.parse varP "x" ~?= Right (Var "x"),
        P.parse valP "42" ~?= Right (Val (IntVal 42)),
        P.parse op1P "-x" ~?= Right (Op1 Neg (Var "x")),
        P.parse op2P "x + 1" ~?= Right (Op2 (Var "x") Plus (Val (IntVal 1))),
        P.parse listConstP "[1; 2; 3]" ~?= Right (ListConst [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)]),
        P.parse tupleConstP "(1, x)" ~?= Right (TupleConst [Val (IntVal 1), Var "x"]),
        P.parse functionConstP "fun x y -> x + y"
          ~?= Right (FunctionConst "x" (FunctionConst "y" (Op2 (Var "x") Plus (Var "y")))),
        P.parse functionConstP "fun x -> fun y -> x + y"
          ~?= Right (FunctionConst "x" (FunctionConst "y" (Op2 (Var "x") Plus (Var "y")))),
        P.parse ifP "if true then 1 else 0"
          ~?= Right (If (Val (BoolVal True)) (Val (IntVal 1)) (Val (IntVal 0))),
        P.parse letP "let x = 1 in x + 1"
          ~?= Right (Let "x" (Val (IntVal 1)) (Op2 (Var "x") Plus (Val (IntVal 1)))),
        P.parse matchP "match x with | [] -> 1 | x::xs -> 2"
          ~?= Right
            ( Match
                (Var "x")
                [ (ListPat [], Val (IntVal 1)),
                  (ConsPat (IdentifierPat "x") (IdentifierPat "xs"), Val (IntVal 2))
                ]
            )
      ]

--- Patterns
--- Patterns
patternP :: Parser Pattern
patternP = wsP (P.char '|') *> choice [
  intConstPatP,
  boolConstPatP,
  listPatP,
  consPatP,
  tuplePatP,
  identifierPatP
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
listPatP = ListPat <$> brackets (wsP patternP `P.sepBy1` wsP (P.char ';'))

consPatP :: Parser Pattern
consPatP = ConsPat <$> wsP patternP <*> (wsP (stringP "::") *> patternP)

tuplePatP :: Parser Pattern
tuplePatP = TuplePat <$> parens (wsP patternP `P.sepBy1` wsP (P.char ','))

wildcardPatP :: Parser Pattern
wildcardPatP = undefined

test_pattern :: Test
test_pattern =
  "parsing patterns"
    ~: TestList
      [ P.parse patternP "| 42" ~?= Right (IntConstPat 42),
        P.parse patternP "| true" ~?= Right (BoolConstPat True),
        P.parse patternP "| x" ~?= Right (IdentifierPat "x"),
        P.parse patternP "| [x; y; z]" ~?= Right (ListPat [IdentifierPat "x", IdentifierPat "y", IdentifierPat "z"]),
        P.parse patternP "| x::xs" ~?= Right (ConsPat (IdentifierPat "x") (IdentifierPat "xs")),
        P.parse patternP "| (x, y)" ~?= Right (TuplePat [IdentifierPat "x", IdentifierPat "y"]),
        P.parse patternP "| _" ~?= Right WildcardPat
      ]

-- >>> P.parse patternP "| x::xs"
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

parseOcamlExp :: String -> Either ParseError Expression
parseOcamlExp = undefined

parseOcamlStat :: String -> Either ParseError Statement
parseOcamlStat = undefined

parseOcaml :: String -> IO (Either ParseError Block)
parseOcaml = undefined
