{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Text.PDDL.Parse where

import           Text.PDDL.Location ( Located(..), SrcLoc(NoLoc) )
import qualified Text.PDDL.PP as PP
import           Text.PDDL.SExp ( SExp(..), ParseError(..) )
import           Text.PDDL.Types

import           Control.Applicative ( Applicative(..), Alternative(..) )
import           MonadLib



-- Parser Monad ----------------------------------------------------------------

newtype Parser a = Parser { unParser :: ReaderT SrcLoc
                                        (ExceptionT ParseError Id) a
                          } deriving (Functor,Applicative,Monad)

instance Alternative Parser where
  empty   = parseError PP.empty
  a <|> b = Parser $ do e <- try (unParser a)
                        case e of
                          Right x -> return x
                          _       -> unParser b

runParser :: Parser a -> Either ParseError a
runParser m = runM (unParser m) NoLoc

withLoc :: Located a -> (a -> Parser b) -> Parser b
withLoc Located { .. } k = Parser 
                         $ local locRange
                         $ unParser
                         $ k locValue

parseError :: PP.Doc -> Parser a
parseError msg = Parser $
  do loc <- ask
     raise (ParseError loc msg)

type Parse a = SExp -> Parser a


-- PDDL ------------------------------------------------------------------------

parsePDDL :: Parse PDDL
parsePDDL sexp = PDDLDomain `fmap` parseDomain sexp

isDefine :: Parse ()
isDefine (SLoc loc)       = withLoc loc isDefine
isDefine (SName "define") = return ()
isDefine _                = parseError (PP.text "expected `define`")

-- Domain Definition -----------------------------------------------------------

parseDomain :: Parse Domain
parseDomain (SLoc loc) =
  withLoc loc parseDomain

parseDomain (SList (def : rest)) =
  do isDefine def
     undefined

parseDomain _ =
  parseError $ PP.text "expected a domain"
