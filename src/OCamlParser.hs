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
import Control.Applicative (liftA2)

-- Helper Parsers
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

parens :: Parser a -> Parser a
parens x = P.between (P.char '(') x (P.char ')') <|> P.between (P.string "begin") x (P.string "end")

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
    "begin",
    "match",
    "let",
    "mod"
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
valP = Val <$> (intValP <|> boolValP <|> functionValP)

expP :: Parser Expression
expP = boolP
  where
    boolP = compP `P.chainl1` opAtLevel (level And)
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Append)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP = applyP <|> Op1 <$> uopP <*> uopexpP
    applyP = foldl1 Apply <$> some baseP
    baseP = choice [wsP (parens expP),
                    tupleConstP, 
                    listConstP,
                    functionConstP,
                    ifP,
                    letP,
                    matchP,
                    varP,
                    valP]

-- >>> parse expP "begin match 2 with 2 -> 2 end"
-- Left "No parses"

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
    foldParams [x] ex = Val (FunctionVal x ex)
    foldParams (x : xs) ex = Val (FunctionVal x (foldr (\x acc -> Val (FunctionVal x acc)) ex xs))

ifP :: Parser Expression
ifP = If <$> (wsP (stringP "if") *> expP) <*> (wsP (stringP "then") *> expP) <*> (wsP (stringP "else") *> expP)

matchP :: Parser Expression
matchP = Match 
    <$> (wsP (stringP "begin match") *> wsP expP <* wsP (stringP "with")) 
    <*> many (wsP patExpP) <* wsP (stringP "end") where
  patExpP :: Parser (Pattern, Expression)
  patExpP = wsP (liftA2 (,) (wsP (P.char '|') *> wsP topLevelPatternP) (wsP (stringP "->") *> wsP expP))

letP :: Parser Expression
letP = Let <$> (wsP (stringP "let") *> idP <* wsP (P.char '=')) <*> wsP expP <*> (wsP (stringP "in") *> expP)

--- Patterns
topLevelPatternP :: Parser Pattern
topLevelPatternP = wsP
  (choice [
    consPatNormalP,
    tuplePatP,
    listPatP,
    wildcardPatP,
    boolConstPatP,
    intConstPatP,
    identifierPatP
  ])

otherPatternP :: Parser Pattern
otherPatternP = wsP (choice [
    tuplePatP,
    listPatP,
    wildcardPatP,
    boolConstPatP,
    intConstPatP,
    identifierPatP
  ])

intConstPatP :: Parser Pattern
intConstPatP = IntConstPat <$> wsP P.int

boolConstPatP :: Parser Pattern
boolConstPatP = BoolConstPat <$> wsP parseBool
  where
    parseBool = P.choice [constP "true" True, constP "false" False]

identifierPatP :: Parser Pattern
identifierPatP = IdentifierPat <$> idP

listPatP :: Parser Pattern
listPatP = ListPat <$> brackets (wsP topLevelPatternP `P.sepBy` wsP (P.char ';'))

consPatP :: Parser Pattern
consPatP = consPatNormalP

-- >>> PP.pretty (Match (Var "XY") [(WildcardPat,ListConst [ListConst [Var "X",Var "xy"],Op1 Neg (Var "y"),Match (Var "xy") [(IntConstPat (-8),Val (BoolVal True)),(BoolConstPat False,Val (IntVal 7))]]),(ConsPat (ConsPat (BoolConstPat False) (IntConstPat 9)) (IdentifierPat "X0"),Let "xy" (Let "x" (Val (BoolVal False)) (Val (IntVal 5))) (TupleConst [Var "X0",Val (IntVal (-3)),Var "X0"]))])
-- "begin match XY with | _ -> [[X; xy]; -(y); begin match xy with | -8 -> true\n | false -> 7\n end]\n | false::9::X0 -> let xy = let x = false in\n 5 in\n (X0, -3, X0)\n end"

-- >>> P.parse expP "begin match XY with | _ -> [[X; xy]; -(y); begin match xy with | -8 -> true\n | false -> 7\n end]\n | false::9::X0 -> let xy = let x = false in\n 5 in\n (X0, -3, X0)\n end"
-- Right (Match (Var "XY") [(WildcardPat,ListConst [ListConst [Var "X",Var "xy"],Op1 Neg (Var "y"),Match (Var "xy") [(IntConstPat (-8),Val (BoolVal True)),(BoolConstPat False,Val (IntVal 7))]]),(ConsPat (BoolConstPat False) (ConsPat (IntConstPat 9) (IdentifierPat "X0")),Let "xy" (Let "x" (Val (BoolVal False)) (Val (IntVal 5))) (TupleConst [Var "X0",Val (IntVal (-3)),Var "X0"]))])


-- >>> P.parse expP "begin match x with | (3,true)::false -> x0\n | true::-2::[true;true] -> X0\n end"
-- Right (Match (Var "x") [(ConsPat (TuplePat [IntConstPat 3,BoolConstPat True]) (BoolConstPat False),Var "x0"),(ConsPat (BoolConstPat True) (ConsPat (IntConstPat (-2)) (ListPat [BoolConstPat True,BoolConstPat True])),Var "X0")])


-- >>> PP.pretty (Match (Var "y") [(ConsPat (ConsPat (IntConstPat 5) (BoolConstPat True)) (ConsPat (IntConstPat 3) (IntConstPat 1)),Var "X0")])
-- "begin match y with | 5::true::3::1 -> X0\n end"

-- >>> P.parse expP "begin match y with | 5::true::3::1 -> X0\n end"
-- Right (Match (Var "y") [(ConsPat (IntConstPat 5) (ConsPat (BoolConstPat True) (ConsPat (IntConstPat 3) (IntConstPat 1))),Var "X0")])

consPatNormalP :: Parser Pattern
consPatNormalP = makeCons <$> otherPatternP <*> many (wsP (stringP "::") *> otherPatternP)
  where
    makeCons p [] = p
    makeCons p (x:xs) = ConsPat p (makeCons x xs)

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

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP (pretty s) == Right s

prop_roundtrip_pat :: Pattern -> Bool
prop_roundtrip_pat e = parse topLevelPatternP (pretty e) == Right e

-- >>> PP.pretty (Match (Var "xy") [(TuplePat [ListPat [BoolConstPat False,IntConstPat (-3)],ListPat [IntConstPat 1,BoolConstPat True,BoolConstPat False]],Var "x0"),(ListPat [IdentifierPat "x",ConsPat (IntConstPat (-3)) (IntConstPat 1)],Var "x")])
-- "begin match xy with | ([false;-3],[1;true;false]) -> x0\n | [x;-3::1] -> x\n  end"

-- >>> P.parse expP "begin match xy with | [x;x] -> x\n | ([false;-3],[1;true;false]) -> x0\n end"
-- Right (Match (Var "xy") [(ListPat [IdentifierPat "x",IdentifierPat "x"],Var "x"),(TuplePat [ListPat [BoolConstPat False,IntConstPat (-3)],ListPat [IntConstPat 1,BoolConstPat True,BoolConstPat False]],Var "x0")])

-- >>> P.parse consPatP "x::xs"
-- Right (ConsPat (IdentifierPat "x") (IdentifierPat "xs"))

-- >>> P.parse consPatP "x::xsx::xss"
-- Right (ConsPat (ConsPat (IdentifierPat "x") (IdentifierPat "xsx")) (IdentifierPat "xss"))
