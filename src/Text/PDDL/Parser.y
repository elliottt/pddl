{
-- vim: ft=haskell

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.PDDL.Parser where

import           Text.PDDL.Core ( Parser, lexer, parseError )
import           Text.PDDL.Lexer ( Lexeme, Token(..), Keyword(..) )
import           Text.PDDL.Location ( Located(..) )
import qualified Text.PDDL.Types as T

}

%token

  -- s-expressions
  '('      { Located $$ (TKeyword Klparen) }
  ')'      { Located $$ (TKeyword Krparen) }
  'define' { Located $$ (TName "define")   }


%monad     { Parser } { (>>=) } { return }
%error     { parseError }

%tokentype { Lexeme }
%lexer     { lexer  } { Located mempty TEof }

%name parsePddl pddl

%%


-- PDDL ------------------------------------------------------------------------

pddl :: { [T.PDDL] }
  : list(pddl_element) { $1 }

pddl_element :: { T.PDDL }
  : problem { T.PDDLProblem $1 }
  | domain  { T.PDDLDomain  $1 }


-- Problems --------------------------------------------------------------------

problem :: { T.Problem }
  : error { undefined }


-- Domains ---------------------------------------------------------------------

domain :: { T.Domain }
  : '(' 'define' { undefined }


-- Utilities -------------------------------------------------------------------

list(p)
  : list_body(p) { reverse $1 }

list1(p)
  : list_body(p) { reverse $1 }
  | p            { $1         }

list_body(p)
  : list_body(p) p { $2 : $1 }
  | {- empty -}    { []      }
