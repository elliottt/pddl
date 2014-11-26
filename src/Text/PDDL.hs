module Text.PDDL (
    module Text.PDDL
  , module Text.PDDL.Types
  , SrcLoc(..)
  , Located(..)
  , ParseError(..)
  ) where

import           Text.PDDL.Location ( Source, SrcLoc(..), Located(..) )
import           Text.PDDL.Parse ( runParser, parsePDDL )
import           Text.PDDL.SExp ( ParseError(..), parseSExp )
import           Text.PDDL.Types

import qualified Data.Text.Lazy as L


parseText :: Source -> L.Text -> Either ParseError PDDL
parseText src bytes = case parseSExp src bytes of
  Right sexp -> runParser (parsePDDL sexp)
  Left err   -> Left err
