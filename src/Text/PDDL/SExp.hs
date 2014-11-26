{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Text.PDDL.SExp (
    SExp(..)
  , parseSExp
  , ParseError(..)
  ) where

import           Control.Applicative ( Applicative(..) )
import           Text.PDDL.Location
                     ( Source, SrcLoc(..), Position(..), Range(..)
                     , Located(..), movePos, at )
import           Text.PDDL.PP
                     ( PP(..), Doc, text, (<+>), sep, (<>), doubleQuotes
                     , isEmpty )
import qualified Text.PDDL.PP as PP

import           Control.Monad ( unless )
import           Data.Char ( isSpace, isNumber, isLetter, isSymbol )
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import           MonadLib ( StateT, get, set, ExceptionT, raise, Id, runM )


-- External Interface ----------------------------------------------------------

data SExp = SList [SExp]
            -- ^ Lists
          | SSym !S.Text
            -- ^ Symbols
          | SName !S.Text
            -- ^ Names
          | SVar !S.Text
            -- ^ Variables (names with a '?' prefix)
          | SNum !Double
            -- ^ Numbers
          | SString !S.Text
            -- ^ Quoted strings
          | SLoc !(Located SExp)
            -- ^ S-expressions with source location information
            deriving (Show)

instance PP SExp where
  pp (SList es)  = pp '(' <> sep (map pp es) <> pp ')'
  pp (SSym t)    = pp t
  pp (SName n)   = pp n
  pp (SVar n)    = pp '?' <> pp n
  pp (SNum d)    = pp d
  pp (SString s) = doubleQuotes (pp s)
  pp (SLoc loc)  = pp loc

data ParseError = ParseError SrcLoc Doc
                  deriving (Show)

instance PP ParseError where
  pp (ParseError loc msg) = pp loc <> PP.char ':' <+> body
    where
    body | isEmpty msg = text "Parse error"
         | otherwise   = msg


-- | Parse an s-expression from a lazy ByteString, with source information.
parseSExp :: Source -> L.Text -> Either ParseError SExp
parseSExp src inp =
  case runM (unParser sexp) rw of
    Right (a,_) -> Right a
    Left err    -> Left err

  where
  rw = RW { rwInput = inp, rwSource = src, rwPos = Position 0 1 1 }


-- Parser ----------------------------------------------------------------------

newtype Parser a = Parser { unParser :: StateT RW (ExceptionT ParseError Id) a
                          } deriving (Functor,Applicative,Monad)

data RW = RW { rwInput  :: L.Text
             , rwSource :: Source
             , rwPos    :: !Position
             }

getPos :: Parser Position
getPos  = Parser $
  do RW { .. } <- get
     return rwPos

getLoc :: Position -> Parser SrcLoc
getLoc start = Parser $
  do RW { .. } <- get
     return (SrcLoc (Range start rwPos) rwSource)

peek :: Parser Char
peek  = Parser $
  do RW { .. } <- get
     case L.uncons rwInput of
       Just (c,_) -> return c
       Nothing    -> raise $ ParseError (SrcLoc (Range rwPos rwPos) rwSource)
                                        (text "Unexpected end of input")

char :: Parser Char
char  = Parser $
  do RW { .. } <- get
     case L.uncons rwInput of
       Just (c,rest) -> do set $! RW { rwInput = rest
                                     , rwPos   = movePos rwPos c
                                     , ..
                                     }
                           return c
       Nothing       -> raise $ ParseError (SrcLoc (Range rwPos rwPos) rwSource)
                                           (text "Unexpected end of input")

-- | Consume until the next character begins something interesting.  As this
-- will fail if it doesn't find anything interesting, it should only be used in
-- cases where the grammar expects something.
trim :: Parser ()
trim  =
  do c <- peek
     if | isSpace c -> do _ <- char
                          trim
        | c == ';'  -> comment >> trim
        | otherwise -> return ()

sexp :: Parser SExp
sexp  =
  do trim
     start <- getPos
     c     <- char
     e     <- if | c == '('   ->  SList             `fmap` list
                 | c == '"'   -> (SString . S.pack) `fmap` string
                 | c == ':'   -> (SSym    . S.pack) `fmap` name
                 | c == '?'   -> (SVar    . S.pack) `fmap` name
                 | isNumber c -> do rest <- number
                                    return (SNum (read (c:rest)))
                 | isLetter c -> do str <- nameBody c
                                    return (SName (S.pack str))
                 | otherwise  -> do rest <- symbol
                                    return (SSym (S.pack (c:rest)))

     loc <- getLoc start
     return (SLoc (e `at` loc))

list :: Parser [SExp]
list  =
  do trim
     c <- peek
     if | c == ')'  -> return []
        | otherwise -> do e  <- sexp
                          es <- list
                          return (e:es)

string :: Parser String
string  =
  do c <- char
     if c == '"'
        then return []
        else do cs <- string
                return (c:cs)

comment :: Parser ()
comment  =
  do c <- char
     if c == '\n'
        then return ()
        else comment

name :: Parser String
name  =
  do l <- char
     unless (isLetter l) (fail "expected a letter")
     nameBody l

nameBody :: Char -> Parser String
nameBody l =
  do rest <- loop
     return (l:rest)
  where
  loop = do c <- peek
            if isLetter c || isNumber c || c `elem` "-_"
               then do _    <- char
                       rest <- loop
                       return (c:rest)
               else return ""

number :: Parser String
number  =
  do c <- peek
     if | isNumber c -> do _    <- char
                           rest <- number
                           return (c:rest)
        | c == '.'   -> do _    <- char
                           rest <- decimal
                           return (c:rest)
        | otherwise  -> return ""


decimal :: Parser String
decimal  =
  do c <- peek
     if isNumber c
        then do _    <- char
                rest <- decimal
                return (c:rest)
        else return ""


symbol :: Parser String
symbol  =
  do c <- peek
     if isSymbol c
        then do _    <- char
                rest <- symbol
                return (c:rest)
        else return ""
