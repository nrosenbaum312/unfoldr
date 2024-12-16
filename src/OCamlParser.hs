{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module OCamlParser where

import Control.Exception (PatternMatchFail)
import GHC.Base (many, undefined, (<|>), liftA3, some)
import GHC.Generics (Par1)
import OCamlPrettyPrinter as PP
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
parens x = (P.between (P.char '(') x (P.char ')')) <|> (P.between (P.string "begin") x (P.string "end"))

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
tupleValP = TupleVal <$> parens (wsP valueP `P.sepBy` wsP (P.char ','))

listValP :: Parser Value
listValP = ListVal <$> brackets (wsP valueP `P.sepBy` wsP (P.char ';'))

functionValP :: Parser Value
functionValP =
  foldParams <$> (wsP (stringP "fun") *> some (wsP idP)) <*> (wsP (stringP "->") *> expP)
  where
    foldParams :: [Identifier] -> Expression -> Value
    foldParams [] _ = error "No params"
    foldParams [x] ex = FunctionVal x ex
    foldParams (x : xs) ex = FunctionVal x (foldr FunctionConst ex xs)

--- Identifiers
idP :: Parser Identifier
idP = P.filter (`notElem` reserved) (wsP ((:) <$> (P.satisfy Char.isAlpha <|> P.char '_') <*>
  many (P.satisfy Char.isAlphaNum <|> P.char '_')))

-- Expression

varP :: Parser Expression
varP = Var <$> idP

valP :: Parser Expression
valP = Val <$> (intValP <|> boolValP)

expP :: Parser Expression
expP = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Append)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP = baseP <|> Op1 <$> uopP <*> uopexpP
    baseP = choice [wsP (parens expP),
                    tupleConstP, 
                    listConstP,
                    functionConstP,
                    ifP,
                    letP,
                    matchP,
                    varP,
                    valP]

opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

listConstP :: Parser Expression
listConstP = ListConst <$> brackets (wsP expP `P.sepBy` wsP (P.char ';'))

tupleConstP :: Parser Expression
tupleConstP =
  ( \elts -> case elts of
      [] -> TupleConst []
      [x] -> x
      l -> TupleConst elts
  )
    <$> parens (wsP (expP `P.sepBy` wsP (P.char ',')))

functionConstP :: Parser Expression
functionConstP =
  foldParams <$> (wsP (stringP "fun") *> some (wsP idP)) <*> (wsP (stringP "->") *> expP)
  where
    foldParams :: [Identifier] -> Expression -> Expression
    foldParams [] _  = error "No params"
    foldParams [x] ex = FunctionConst x ex
    foldParams (x : xs) ex = FunctionConst x (foldr FunctionConst ex xs)

ifP :: Parser Expression
ifP = If <$> (wsP (stringP "if") *> expP) <*> (wsP (stringP "then") *> expP) <*> (wsP (stringP "else") *> expP)

matchP :: Parser Expression
matchP = Match 
    <$> (wsP (stringP "begin match") *> wsP expP <* wsP (stringP "with")) 
    <*> many (wsP patExpP) <* wsP (stringP "end") <* wsP (stringP "end") where
  patExpP :: Parser (Pattern, Expression)
  patExpP = wsP (liftA2 (,) (wsP topLevelPatternP) (wsP (stringP "->") *> wsP expP))

letP :: Parser Expression
letP = Let <$> (wsP (stringP "let") *> idP <* wsP (P.char '=')) <*> wsP expP <*> (wsP (stringP "in") *> expP)

applyP :: Parser Expression
applyP = Apply <$> wsP expP <*> wsP expP

--- Patterns
topLevelPatternP :: Parser Pattern
topLevelPatternP = wsP (P.char '|') *>
  choice [
    tuplePatP,
    intConstPatP,
    boolConstPatP,
    wildcardPatP,
    listPatP,
    consPatP,
    identifierPatP
  ]

otherPatternP :: Parser Pattern
otherPatternP = choice [
  intConstPatP,
  boolConstPatP,
  wildcardPatP,
  listPatP,
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
listPatP = ListPat <$> brackets (wsP otherPatternP `P.sepBy` wsP (P.char ';'))

consPatP :: Parser Pattern
consPatP = otherPatternP `P.chainl1` consOp
  where
    consOp = ConsPat <$ wsP (stringP "::")

tuplePatP :: Parser Pattern
tuplePatP = 
  ( \elts -> case elts of
      [] -> TuplePat []
      [x] -> x
      l -> TuplePat elts
  )
    <$> parens (wsP (topLevelPatternP `P.sepBy` wsP (P.char ',')))

wildcardPatP :: Parser Pattern
wildcardPatP = WildcardPat <$ wsP (P.char '_')


--- OPS
bopP :: Parser Bop
bopP = wsP (Plus <$ P.char '+' <|> Minus <$ P.char '-' <|> Times <$ P.char '*' <|> Divide <$ P.string "/" <|> Mod <$ P.string "mod"
        <|> Eq <$ P.string "=" <|> Ge <$ P.string ">=" <|> Gt <$ P.char '>' <|> Le <$ P.string "<=" <|> Lt <$ P.char '<' <|> Append <$ P.char '@' <|> Cons <$ P.string "::"
        <|> Or <$ P.string "||" <|> And <$ P.string "&&")

uopP :: Parser Uop
uopP =  P.choice [constP "-" Neg, constP "not" Not]


--- statements
statementP :: Parser Statement
statementP = varDeclP 

varDeclP :: Parser Statement
varDeclP = VarDecl <$> wsP isRec <*> wsP idP <*> (wsP (P.char '=') *> wsP expP)
  where
    isRec = (True <$ wsP (P.string "let rec")) <|> (False <$ wsP (P.string "let"))

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

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP (pretty s) == Right s

prop_roundtrip_pat :: Pattern -> Bool
prop_roundtrip_pat e = parse topLevelPatternP (pretty e) == Right e

-- >>> PP.pretty (ListConst [Op2 (Val (BoolVal False)) Or (Val (BoolVal False)),TupleConst [Var "x",Var "xy",Val (BoolVal True)],Var "x"])
-- "[(false) || (false); (x, xy, true); x]"

-- >>> P.parse expP "[false mod true; x; x]"
-- Right (ListConst [Op2 (Val (BoolVal False)) Mod (Val (BoolVal True)),Var "x",Var "x"])

-- >>> P.parse expP "false || true"
-- Right (Val (BoolVal False))
