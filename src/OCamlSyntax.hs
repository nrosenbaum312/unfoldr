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
  | FunctionVal [Identifier] Expression -- fun x -> x * 2
  deriving (Eq, Show)

type Scope = Map Identifier Value

data Expression
  = Var Identifier -- variables, in any scope
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | ListConst [Expression] -- list construction, like [1; 2; 3]
  | TupleConst [Expression] -- tuple construction, like (1, 2, 3)
  | FunctionConst [Identifier] Expression -- function construction, like fun x -> x * 2
  | If Expression Expression Expression -- If Expression then Expression else Expression
  | Match Expression [(Pattern, Expression)] -- match expression, like match x with | [] -> 1 | x::xs -> 2
  | Let Identifier Expression Expression -- let identifier = expression in expression
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
  = FunctionDecl Bool Identifier [Identifier] Expression
  | VarDecl Identifier Expression
  | Empty -- ';'
  deriving (Eq, Show)

-- var :: String -> Expression
-- var = Var . Name

-- -- test.lu
-- wTest :: Block
-- wTest =
--   Block
--     [ Assign
--         (Name "x")
--         ( Op2
--             ( Op2
--                 (Op2 (Val (IntVal 1)) Plus (Val (IntVal 2)))
--                 Minus
--                 (Val (IntVal 3))
--             )
--             Plus
--             (Op2 (Val (IntVal 1)) Plus (Val (IntVal 3)))
--         ),
--       Assign (Name "y") (Val (IntVal 0)),
--       While
--         (Op2 (var "x") Gt (Val (IntVal 0)))
--         ( Block
--             [ Assign (Name "y") (Op2 (var "y") Plus (var "x")),
--               Assign (Name "x") (Op2 (var "x") Minus (Val (IntVal 1)))
--             ]
--         )
--     ]

-- -- fact.lu
-- wFact :: Block
-- wFact =
--   Block
--     [ Assign (Name "n") (Val (IntVal 5)),
--       Assign (Name "f") (Val (IntVal 1)),
--       While
--         (Op2 (var "n") Gt (Val (IntVal 0)))
--         ( Block
--             [ Assign (Name "x") (Var (Name "n")),
--               Assign (Name "z") (Var (Name "f")),
--               While
--                 (Op2 (var "x") Gt (Val (IntVal 1)))
--                 ( Block
--                     [ Assign (Name "f") (Op2 (var "z") Plus (Var (Name "f"))),
--                       Assign (Name "x") (Op2 (var "x") Minus (Val (IntVal 1)))
--                     ]
--                 ),
--               Assign (Name "n") (Op2 (Var (Name "n")) Minus (Val (IntVal 1)))
--             ]
--         )
--     ]

-- -- abs.lu
-- wAbs :: Block
-- wAbs =
--   Block
--     [ Assign (Name "x") (Op2 (Val (IntVal 0)) Minus (Val (IntVal 3))),
--       If
--         (Op2 (Var (Name "x")) Lt (Val (IntVal 0)))
--         (Block [Assign (Name "x") (Op2 (Val (IntVal 0)) Minus (Var (Name "x")))])
--         (Block [])
--     ]

-- -- times.lu
-- wTimes :: Block
-- wTimes =
--   Block
--     [ Assign (Name "x") (Val (IntVal 10)),
--       Assign (Name "y") (Val (IntVal 3)),
--       Assign (Name "z") (Val (IntVal 0)),
--       While
--         (Op2 (Var (Name "x")) Gt (Val (IntVal 0)))
--         ( Block
--             [ Assign (Name "z") (Op2 (Var (Name "z")) Plus (Var (Name "y"))),
--               Assign (Name "x") (Op2 (Var (Name "x")) Minus (Val (IntVal 1)))
--             ]
--         )
--     ]

-- -- table.lu
-- wTable :: Block
-- wTable = Block [Assign (Name "a") (TableConst []), Assign (Name "k") (Val (StringVal "x")), Assign (Proj (Var (Name "a")) (Var (Name "k"))) (Val (IntVal 10)), Assign (Proj (Var (Name "a")) (Val (IntVal 20))) (Val (StringVal "great")), Assign (Name "o1") (Var (Proj (Var (Name "a")) (Val (StringVal "x")))), Assign (Name "k") (Val (IntVal 20)), Assign (Name "o2") (Var (Proj (Var (Name "a")) (Var (Name "k")))), Assign (Proj (Var (Name "a")) (Val (StringVal "x"))) (Op2 (Var (Proj (Var (Name "a")) (Val (StringVal "x")))) Plus (Val (IntVal 1))), Assign (Name "o3") (Var (Proj (Var (Name "a")) (Val (StringVal "x"))))]

-- -- bfs.lu
-- wBfs :: Block
-- wBfs = Block [Assign (Name "start") (Val (IntVal 1)), Assign (Name "goal") (Val (IntVal 10)), Empty, Assign (Name "graph") (TableConst []), Assign (Proj (Var (Name "graph")) (Val (IntVal 1))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 2)), FieldKey (Val (IntVal 2)) (Val (IntVal 3))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 2))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 6)), FieldKey (Val (IntVal 2)) (Val (IntVal 5)), FieldKey (Val (IntVal 3)) (Val (IntVal 1))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 3))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 1))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 4))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 7)), FieldKey (Val (IntVal 2)) (Val (IntVal 8))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 5))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 9)), FieldKey (Val (IntVal 2)) (Val (IntVal 10)), FieldKey (Val (IntVal 3)) (Val (IntVal 2))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 6))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 2))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 7))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 4)), FieldKey (Val (IntVal 2)) (Val (IntVal 11)), FieldKey (Val (IntVal 3)) (Val (IntVal 12))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 8))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 4))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 9))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 5))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 10))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 5))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 11))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 7))]), Assign (Proj (Var (Name "graph")) (Val (IntVal 12))) (TableConst [FieldKey (Val (IntVal 1)) (Val (IntVal 7))]), Empty, Assign (Name "q") (TableConst [FieldName "elements" (TableConst []), FieldName "first" (Val (IntVal 0)), FieldName "last" (Val (IntVal 0))]), Assign (Proj (Var (Dot (Var (Name "q")) "elements")) (Var (Dot (Var (Name "q")) "last"))) (Var (Name "start")), Assign (Dot (Var (Name "q")) "last") (Op2 (Var (Dot (Var (Name "q")) "last")) Plus (Val (IntVal 1))), Empty, Assign (Name "visited") (TableConst []), Assign (Proj (Var (Name "visited")) (Var (Name "start"))) (Val (BoolVal True)), Assign (Name "found") (Val (BoolVal False)), Empty, Repeat (Block [Assign (Name "node") (Var (Proj (Var (Dot (Var (Name "q")) "elements")) (Var (Dot (Var (Name "q")) "first")))), Assign (Proj (Var (Dot (Var (Name "q")) "elements")) (Var (Dot (Var (Name "q")) "first"))) (Val NilVal), Assign (Dot (Var (Name "q")) "first") (Op2 (Var (Dot (Var (Name "q")) "first")) Plus (Val (IntVal 1))), Empty, Assign (Proj (Var (Name "visited")) (Var (Name "node"))) (Val (BoolVal True)), If (Op2 (Var (Name "goal")) Eq (Var (Name "node"))) (Block [Assign (Name "found") (Val (BoolVal True)), Assign (Dot (Var (Name "q")) "first") (Var (Dot (Var (Name "q")) "last"))]) (Block [Assign (Name "i") (Val (IntVal 1)), While (Op2 (Var (Name "i")) Le (Op1 Len (Var (Proj (Var (Name "graph")) (Var (Name "node")))))) (Block [Assign (Name "next") (Var (Proj (Var (Proj (Var (Name "graph")) (Var (Name "node")))) (Var (Name "i")))), If (Op1 Not (Var (Proj (Var (Name "visited")) (Var (Name "next"))))) (Block [Assign (Proj (Var (Dot (Var (Name "q")) "elements")) (Var (Dot (Var (Name "q")) "last"))) (Var (Name "next")), Assign (Dot (Var (Name "q")) "last") (Op2 (Var (Dot (Var (Name "q")) "last")) Plus (Val (IntVal 1)))]) (Block [Empty]), Assign (Name "i") (Op2 (Var (Name "i")) Plus (Val (IntVal 1)))])])]) (Op2 (Op2 (Var (Dot (Var (Name "q")) "last")) Minus (Var (Dot (Var (Name "q")) "first"))) Eq (Val (IntVal 0)))]

-- -- >>> wTest
-- -- Block [Assign (Name "x") (Op2 (Op2 (Op2 (Val (IntVal 1)) Plus (Val (IntVal 2))) Minus (Val (IntVal 3))) Plus (Op2 (Val (IntVal 1)) Plus (Val (IntVal 3)))),Assign (Name "y") (Val (IntVal 0)),While (Op2 (Var (Name "x")) Gt (Val (IntVal 0))) (Block [Assign (Name "y") (Op2 (Var (Name "y")) Plus (Var (Name "x"))),Assign (Name "x") (Op2 (Var (Name "x")) Minus (Val (IntVal 1)))])]

-- -- >>> pretty wTest

-- class PP a where
--   pp :: a -> Doc

-- -- | Compact version. Displays its argument without newlines.
-- oneLine :: (PP a) => a -> String
-- oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

-- instance PP Uop where
--   pp Neg = PP.char '-'
--   pp Not = PP.text "not"
--   pp Len = PP.char '#'

-- instance PP Bool where
--   pp :: Bool -> Doc
--   pp True = PP.text "true"
--   pp False = PP.text "false"

-- instance PP String where
--   pp :: String -> Doc
--   pp = PP.text

-- instance PP Int where
--   pp :: Int -> Doc
--   pp = PP.int

-- instance PP TableName where
--   pp :: TableName -> Doc
--   pp (TN x) = PP.text x

-- instance PP Var where
--   pp :: Var -> Doc
--   pp (Name n) = PP.text n
--   pp (Dot (Var v) k) = pp v <> PP.text "." <> pp k
--   pp (Dot t k) = PP.parens (pp t) <> PP.text "." <> pp k
--   pp (Proj (Var v) k) = pp v <> PP.brackets (pp k)
--   pp (Proj t k) = PP.parens (pp t) <> PP.brackets (pp k)

-- instance PP Value where
--   pp :: Value -> Doc
--   pp (IntVal i) = pp i
--   pp (BoolVal b) = pp b
--   pp NilVal = PP.text "nil"
--   pp (StringVal s) = PP.text ("\"" <> s <> "\"")
--   pp (TableVal tn) = PP.text "<" <> pp tn <> PP.text ">"

-- isBase :: Expression -> Bool
-- isBase TableConst {} = True
-- isBase Val {} = True
-- isBase Var {} = True
-- isBase Op1 {} = True
-- isBase _ = False

-- instance PP Bop where
--   pp :: Bop -> Doc
--   pp Plus = PP.char '+'
--   pp Minus = PP.char '-'
--   pp Times = PP.char '*'
--   pp Divide = PP.text "//"
--   pp Modulo = PP.text "%"
--   pp Gt = PP.char '>'
--   pp Ge = PP.text ">="
--   pp Lt = PP.char '<'
--   pp Le = PP.text "<="
--   pp Eq = PP.text "=="
--   pp Concat = PP.text ".."

-- instance PP Expression where
--   pp :: Expression -> Doc
--   pp (Var v) = pp v
--   pp (Val v) = pp v
--   pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
--   pp e@Op2 {} = ppPrec 0 e
--     where
--       ppPrec n (Op2 e1 bop e2) =
--         ppParens (level bop < n) $
--           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
--       ppPrec _ e' = pp e'
--       ppParens b = if b then PP.parens else id
--   pp (TableConst fs) = PP.braces (PP.sep (PP.punctuate PP.comma (map pp fs)))

-- instance PP TableField where
--   pp :: TableField -> Doc
--   pp (FieldName name e) = pp name <+> PP.equals <+> pp e
--   pp (FieldKey e1 e2) = PP.brackets (pp e1) <+> PP.equals <+> pp e2

-- instance PP Block where
--   pp :: Block -> Doc
--   pp (Block [s]) = pp s
--   pp (Block ss) = PP.vcat (map pp ss)

-- ppSS :: [Statement] -> Doc
-- ppSS ss = PP.vcat (map pp ss)

-- instance PP Statement where
--   pp :: Statement -> Doc
--   pp (Assign x e) = pp x <+> PP.equals <+> pp e
--   pp (If guard b1 b2) =
--     PP.hang (PP.text "if" <+> pp guard <+> PP.text "then") 2 (pp b1)
--       PP.$$ PP.nest 2 (PP.text "else" PP.$$ pp b2)
--       PP.$$ PP.text "end"
--   pp (While guard e) =
--     PP.hang (PP.text "while" <+> pp guard <+> PP.text "do") 2 (pp e)
--       PP.$+$ PP.text "end"
--   pp Empty = PP.semi
--   pp (Repeat b e) =
--     PP.hang (PP.text "repeat") 2 (pp b)
--       PP.$+$ PP.text "until"
--       <+> pp e

-- level :: Bop -> Int
-- level Times = 7
-- level Divide = 7
-- level Plus = 5
-- level Minus = 5
-- level Concat = 4
-- level _ = 3 -- comparison operators

-- instance (PP a) => PP (Map Value a) where
--   pp :: (PP a) => Map Value a -> Doc
--   pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
--     where
--       ppa (StringVal s, v2) = PP.text s <+> PP.text "=" <+> pp v2
--       ppa (v1, v2) = PP.brackets (pp v1) <+> PP.text "=" <+> pp v2

-- instance (PP a) => PP (Map TableName a) where
--   pp :: (PP a) => Map TableName a -> Doc
--   pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
--     where
--       ppa (s, v2) = pp s <+> PP.text "=" <+> pp v2

-- sampleVar :: IO ()
-- sampleVar = QC.sample' (arbitrary :: Gen Var) >>= mapM_ (print . pp)

-- sampleExp :: IO ()
-- sampleExp = QC.sample' (arbitrary :: Gen Expression) >>= mapM_ (print . pp)

-- sampleStat :: IO ()
-- sampleStat = QC.sample' (arbitrary :: Gen Statement) >>= mapM_ (print . pp)

-- quickCheckN :: (QC.Testable prop) => Int -> prop -> IO ()
-- quickCheckN n = QC.quickCheckWith $ QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}

-- -- | Generate a small set of names for generated tests. These names are guaranteed to not include
-- -- reserved words
-- genName :: Gen Name
-- genName = QC.elements ["x", "X", "y", "x0", "X0", "xy", "XY"]

-- -- | Generate a string literal, being careful about the characters that it may contain
-- genStringLit :: Gen String
-- genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
--   where
--     -- escape special characters appearing in the string,
--     escape :: String -> String
--     escape = foldr Char.showLitChar ""
--     -- generate strings containing printable characters or spaces, but not including '\"'
--     stringLitChars :: [Char]
--     stringLitChars = filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']

-- -- | Generate a size-controlled global variable or table field
-- genVar :: Int -> Gen Var
-- genVar 0 = Name <$> genName
-- genVar n =
--   QC.frequency
--     [ (1, Name <$> genName),
--       (n, Dot <$> genExp n' <*> genName),
--       (n, Proj <$> genExp n' <*> genExp n')
--     ]
--   where
--     n' = n `div` 2

-- -- | Generate a size-controlled expression
-- genExp :: Int -> Gen Expression
-- genExp 0 = QC.oneof [Var <$> genVar 0, Val <$> arbitrary]
-- genExp n =
--   QC.frequency
--     [ (1, Var <$> genVar n),
--       (1, Val <$> arbitrary),
--       (n, Op1 <$> arbitrary <*> genExp n'),
--       (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
--       (n', TableConst <$> genTableFields n')
--     ]
--   where
--     n' = n `div` 2

-- -- | Generate a list of fields in a table constructor epression.
-- -- We limit the size of the table to avoid size blow up.
-- genTableFields :: Int -> Gen [TableField]
-- genTableFields n = do
--   len <- QC.elements [0 .. 3]
--   take len <$> QC.infiniteListOf (genTableField n)

-- genTableField :: Int -> Gen TableField
-- genTableField n =
--   QC.oneof
--     [ FieldName <$> genName <*> genExp n',
--       FieldKey <$> genExp n' <*> genExp n'
--     ]
--   where
--     n' = n `div` 2

-- -- | Generate a size-controlled statement
-- genStatement :: Int -> Gen Statement
-- genStatement n | n <= 1 = QC.oneof [Assign <$> genVar 0 <*> genExp 0, return Empty]
-- genStatement n =
--   QC.frequency
--     [ (1, Assign <$> genVar n' <*> genExp n'),
--       (1, return Empty),
--       (n, If <$> genExp n' <*> genBlock n' <*> genBlock n'),
--       -- generate loops half as frequently as if statements
--       (n', While <$> genExp n' <*> genBlock n'),
--       (n', Repeat <$> genBlock n' <*> genExp n')
--     ]
--   where
--     n' = n `div` 2

-- genBlock :: Int -> Gen Block
-- genBlock n = Block <$> genStmts n
--   where
--     genStmts 0 = pure []
--     genStmts n =
--       QC.frequency
--         [ (1, return []),
--           (n, (:) <$> genStatement n' <*> genStmts n')
--         ]
--       where
--         n' = n `div` 2

-- instance Arbitrary TableName where
--   arbitrary :: Gen TableName
--   arbitrary = QC.elements [TN "_", TN "_G", TN "_x", TN "_t1"]

-- instance Arbitrary Var where
--   arbitrary :: Gen Var
--   arbitrary = QC.sized genVar
--   shrink :: Var -> [Var]
--   shrink (Name n) = []
--   shrink (Dot e n) = [Dot e' n | e' <- shrink e]
--   shrink (Proj e1 e2) =
--     [Proj e1' e2 | e1' <- shrink e1]
--       ++ [Proj e1 e2' | e2' <- shrink e2]

-- instance Arbitrary Statement where
--   arbitrary :: Gen Statement
--   arbitrary = QC.sized genStatement
--   shrink :: Statement -> [Statement]
--   shrink (Assign v e) =
--     [Assign v' e | v' <- shrink v]
--       ++ [Assign v e' | e' <- shrink e]
--   shrink (If e b1 b2) =
--     first b1
--       ++ first b2
--       ++ [If e' b1 b2 | e' <- shrink e]
--       ++ [If e b1' b2 | b1' <- shrink b1]
--       ++ [If e b1 b2' | b2' <- shrink b2]
--   shrink (While e b) =
--     first b
--       ++ [While e' b | e' <- shrink e]
--       ++ [While e b' | b' <- shrink b]
--   shrink Empty = []
--   shrink (Repeat b e) =
--     first b
--       ++ [Repeat b' e | b' <- shrink b]
--       ++ [Repeat b e' | e' <- shrink e]

-- -- | access the first statement in a block, if one exists
-- first :: Block -> [Statement]
-- first (Block []) = []
-- first (Block (x : _)) = [x]

-- -- | access expressions in a table field
-- getExp :: TableField -> [Expression]
-- getExp (FieldName _ e) = [e]
-- getExp (FieldKey e1 e2) = [e1, e2]

-- instance Arbitrary TableField where
--   arbitrary :: Gen TableField
--   arbitrary = QC.sized genTableField
--   shrink :: TableField -> [TableField]
--   shrink (FieldName n e1) = [FieldName n e1' | e1' <- shrink e1]
--   shrink (FieldKey e1 e2) =
--     [FieldKey e1' e2 | e1' <- shrink e1]
--       ++ [FieldKey e1 e2' | e2' <- shrink e2]

-- instance Arbitrary Block where
--   arbitrary :: Gen Block
--   arbitrary = QC.sized genBlock
--   shrink :: Block -> [Block]
--   shrink (Block ss) = [Block ss' | ss' <- shrink ss]

-- instance Arbitrary Expression where
--   arbitrary :: Gen Expression
--   arbitrary = QC.sized genExp

--   shrink :: Expression -> [Expression]
--   shrink (Val v) = Val <$> shrink v
--   shrink (Var v) = Var <$> shrink v
--   shrink (Op1 o e) = e : [Op1 o e' | e' <- shrink e]
--   shrink (Op2 e1 o e2) =
--     [Op2 e1' o e2 | e1' <- shrink e1]
--       ++ [Op2 e1 o e2' | e2' <- shrink e2]
--       ++ [e1, e2]
--   shrink (TableConst fs) = concatMap getExp fs ++ (TableConst <$> shrink fs)

-- instance Arbitrary Uop where
--   arbitrary :: Gen Uop
--   arbitrary = QC.arbitraryBoundedEnum

-- instance Arbitrary Bop where
--   arbitrary :: Gen Bop
--   arbitrary = QC.arbitraryBoundedEnum

-- shrinkStringLit :: String -> [String]
-- shrinkStringLit s = filter (/= '\"') <$> shrink s

-- instance Arbitrary Value where
--   arbitrary :: Gen Value
--   arbitrary =
--     QC.oneof
--       [ IntVal <$> arbitrary,
--         BoolVal <$> arbitrary,
--         pure NilVal,
--         StringVal <$> genStringLit
--         -- note: do not generate table values
--       ]

--   shrink :: Value -> [Value]
--   shrink (IntVal n) = IntVal <$> shrink n
--   shrink (BoolVal b) = BoolVal <$> shrink b
--   shrink NilVal = []
--   shrink (StringVal s) = StringVal <$> shrinkStringLit s
--   shrink (TableVal _) = []
