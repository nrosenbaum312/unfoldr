module OCamlStepper where

import Control.Monad (forM, unless, when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import OCamlParser qualified
import OCamlSyntax (Scope)
import State (State)
import State qualified as S
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

-- make an empty scope
makeScope :: OCamlSyntax.Scope
makeScope = undefined