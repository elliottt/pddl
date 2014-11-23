{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Text.PDDL.Core where

import           Text.PDDL.Lexer
import           Text.PDDL.Location
import           Text.PDDL.PP

import           Control.Applicative ( Applicative )
import           MonadLib ( StateT, get, set, ExceptionT, raise, Id, runM )


newtype Parser a = Parser { unParser :: StateT RW (ExceptionT Error Id) a
                          } deriving (Functor,Applicative,Monad)

runParser :: [Lexeme] -> Parser a -> Either Error a
runParser toks p = case runM (unParser p) (RW toks) of
  Right (a,_) -> Right a
  Left err    -> Left err

data RW = RW { rwTokens :: [Lexeme]
             } deriving (Show)

data Error = Error !SrcLoc
             deriving (Show)

instance PP Error where
  pp (Error loc) = text "Parse error near:" <+> pp loc

parseError :: Lexeme -> Parser a
parseError Located { .. } = Parser (raise (Error locRange))

lexer :: (Lexeme -> Parser a) -> Parser a
lexer k =
  do rw <- Parser get
     case rwTokens rw of

       l:ls -> do Parser (set rw { rwTokens = ls })
                  k l

       [] -> fail "Unexpected end of input"
