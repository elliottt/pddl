-- vim: filetype=haskell

{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Text.PDDL.Lexer (
    Token(..)
  , Keyword(..)
  , Lexeme
  , scan
  ) where

import Text.PDDL.Location

import           Data.Bits (shiftR,(.&.))
import           Data.Int (Int64)
import           Data.Monoid (mempty)
import qualified Data.Text as S
import           Data.Word (Word8)
import           MonadLib
import qualified Data.Text.Lazy as L
}

$digit       = [0-9]
$letter      = [a-zA-Z]
$lowerletter = [a-z]


@anychar     = [ $letter $digit [_ \-] ]
@name        = $letter @anychar*

:-

<0> {

-- skip whitespace
$white          ;
";".*$          ;

-- reserved symbols
"("             { keyword Klparen     }
")"             { keyword Krparen     }
"-"             { keyword Kdash       }

@name                { emitS (TName . S.pack) }
\? @name             { emitS mkVar            }
\: @name             { emitS mkSym            }
$digit+              { emitS (TNum  . read)   }
$digit+ "." $digit+  { emitS (TNum  . read)   }
}

{

-- Input Operations ------------------------------------------------------------

type AlexInput = LexerInput

data LexerInput = LexerInput
  { liPosn   :: !Position
  , liSource :: FilePath
  , liChar   :: !Char
  , liBytes  :: [Word8]
  , liInput  :: L.Text
  } deriving (Show)

initLexerInput :: FilePath -> L.Text -> LexerInput
initLexerInput source bytes = LexerInput
  { liPosn   = Position 0 1 1
  , liSource = source
  , liChar   = '\n'
  , liBytes  = []
  , liInput  = bytes
  }

-- | Build a range from the lexer state.
mkRange :: LexerInput -> String -> SrcLoc
mkRange li str =
  SrcLoc (Range (liPosn li) (movesPos (liPosn li) str)) (Just (liSource li))

fillBuffer :: LexerInput -> Maybe LexerInput
fillBuffer li = do
  (c,rest) <- L.uncons (liInput li)
  return $! li
    { liPosn  = movePos (liPosn li) c
    , liBytes = utf8Encode c
    , liChar  = c
    , liInput = rest
    }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar  = liChar

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte li = case liBytes li of
  b:bs -> return (b, li { liBytes = bs })
  _    -> alexGetByte =<< fillBuffer li

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 +  oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + ( oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 +   oc             .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + ( oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6)  .&. 0x3f)
                        , 0x80 +   oc              .&. 0x3f
                        ]


-- Lexer Monad -----------------------------------------------------------------

newtype Lexer a = Lexer
  { unLexer :: StateT LexerInput Id a
  } deriving (Functor,Monad)

scan :: FilePath -> L.Text -> [Lexeme]
scan source bytes = fst (runId (runStateT st0 (unLexer loop)))
  where
  st0  = initLexerInput source bytes

  loop = do
    inp <- alexGetInput
    case alexScan inp 0 of

      AlexToken inp' len action -> do
        alexSetInput inp'
        mb   <- action inp len
        rest <- loop
        case mb of
          Just lex -> return (lex:rest)
          Nothing  -> return rest

      AlexSkip inp' len -> do
        alexSetInput inp'
        loop

      AlexEOF ->
        return [Located mempty TEof]

      AlexError inp' ->
        return [Located (mkRange inp' "") (TError "Lexical error")]

alexSetInput :: AlexInput -> Lexer ()
alexSetInput ai = Lexer (set ai)

alexGetInput :: Lexer AlexInput
alexGetInput  = Lexer get


-- Actions ---------------------------------------------------------------------

type Lexeme = Located Token

data Token = TKeyword Keyword
           | TName !S.Text
           | TVar !S.Text
           | TSym !S.Text
           | TNum !Double
           | TError String
           | TEof
             deriving (Show)

data Keyword = Klparen
             | Krparen
             | Kdash
               deriving (Show)


type AlexAction result = AlexInput -> Int -> result

-- | Emit a token from the lexer
emitT :: Token -> AlexAction (Lexer (Maybe Lexeme))
emitT tok = emitS (const tok)

emitS :: (String -> Token) -> AlexAction (Lexer (Maybe Lexeme))
emitS mk li len = return (Just $! Located (mkRange li str) (mk str))
  where
  range = mkRange li str
  str   = L.unpack (L.take (fromIntegral len) (liInput li))

keyword :: Keyword -> AlexAction (Lexer (Maybe Lexeme))
keyword kw = emitT (TKeyword kw)

mkVar :: String -> Token
mkVar (_:str) = TVar (S.pack str)

mkSym :: String -> Token
mkSym (_:str) = TSym (S.pack str)

}

