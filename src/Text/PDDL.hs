module Text.PDDL (
    module Text.PDDL
  , module Text.PDDL.Types
  , Error(..)
  ) where

import           Text.PDDL.Core ( runParser, Error(..) )
import           Text.PDDL.Lexer ( scan )
import           Text.PDDL.Parser ( parsePddl )
import           Text.PDDL.Types

import qualified Data.Text.Lazy as L


parseText :: String -> L.Text -> Either Error [PDDL]
parseText src bytes = runParser (scan src bytes) parsePddl
