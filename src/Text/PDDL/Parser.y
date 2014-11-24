{
-- vim: ft=haskell

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Text.PDDL.Parser where

import           Text.PDDL.Core ( Parser, lexer, parseError )
import           Text.PDDL.Lexer ( Lexeme, Token(..), Symbol(..) )
import           Text.PDDL.Location ( Located(..), at, unLoc )
import qualified Text.PDDL.Types as T

import           Control.Applicative ( (<$) )
import           Data.Monoid ( mconcat )

import Debug.Trace

}

%token

  -- s-expressions
  ')' { Located $$ TEnd  }
  '-' { Located $$ TDash }

  -- keywords
  '(define'        { Located $$ (TBegin (SName "define"))       }
  '(domain'        { Located $$ (TBegin (SName "domain"))       }
  '(:requirements' { Located $$ (TBegin (SSym  "requirements")) }
  '(:types'        { Located $$ (TBegin (SSym  "types"))        }
  '(either'        { Located $$ (TBegin (SName "either"))       }

  -- requirements
  ':strips'                    { Located $$ (TSymbol (SSym "strips"))                    }
  ':typing'                    { Located $$ (TSymbol (SSym "typing"))                    }
  ':negative-preconditions'    { Located $$ (TSymbol (SSym "negative-preconditions"))    }
  ':disjunctive-preconditions' { Located $$ (TSymbol (SSym "disjunctive-preconditions")) }
  ':equality'                  { Located $$ (TSymbol (SSym "equality"))                  }
  ':existential-preconditions' { Located $$ (TSymbol (SSym "existential-preconditions")) }
  ':universal-preconditions'   { Located $$ (TSymbol (SSym "universal-preconditions"))   }
  ':conditional-effects'       { Located $$ (TSymbol (SSym "conditional-effects"))       }
  ':fluents'                   { Located $$ (TSymbol (SSym "fluents"))                   }
  ':numeric-fluents'           { Located $$ (TSymbol (SSym "numeric-fluents"))           }
  ':object-fluents'            { Located $$ (TSymbol (SSym "object-fluents"))            }
  ':adl'                       { Located $$ (TSymbol (SSym "adl"))                       }
  ':durative-actions'          { Located $$ (TSymbol (SSym "durative-actions"))          }
  ':duration-inequalities'     { Located $$ (TSymbol (SSym "duration-inequalities"))     }
  ':continuous-effects'        { Located $$ (TSymbol (SSym "continuous-effects"))        }
  ':derived-predicates'        { Located $$ (TSymbol (SSym "derived-predicates"))        }
  ':timed-initial-literals'    { Located $$ (TSymbol (SSym "timed-initial-literals"))    }
  ':preferences'               { Located $$ (TSymbol (SSym "preferences"))               }
  ':constraints'               { Located $$ (TSymbol (SSym "constraints"))               }
  ':action-costs'              { Located $$ (TSymbol (SSym "action-costs"))              }

  NAME { $$ @ (Located _  (TSymbol (SName _)))   }
  VAR  { $$ @ (Located _  (TSymbol (SVar _)))    }


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
  : '(define'
        '(domain' name ')'
        require_def
        types_def
    ')'
     { T.Domain { T.dName     = $3
                , T.dRequires = $5
                , T.dTypes    = $6
                , T.dConsts   = T.UntypedList []
                , T.dPredSigs = []
                , T.dFuns     = T.UntypedList []
                , T.dCons     = []
                , T.dActions  = []
                }
     }


-- Requirements ----------------------------------------------------------------

require_def :: { [Located T.Requirement] }
  : '(:requirements' list1(require_key) ')' { $2 }
  | {- empty -}                             { [] }

require_key :: { Located T.Requirement }
  : ':strips'                    { T.ReqStrips                   `at` $1 }
  | ':typing'                    { T.ReqTyping                   `at` $1 }
  | ':negative-preconditions'    { T.ReqNegativePreconditions    `at` $1 }
  | ':disjunctive-preconditions' { T.ReqDisjunctivePreconditions `at` $1 }
  | ':equality'                  { T.ReqEquality                 `at` $1 }
  | ':existential-preconditions' { T.ReqExistentialPreconditions `at` $1 }
  | ':universal-preconditions'   { T.ReqUniversalPreconditions   `at` $1 }
  | ':conditional-effects'       { T.ReqConditionalEffects       `at` $1 }
  | ':fluents'                   { T.ReqFluents                  `at` $1 }
  | ':numeric-fluents'           { T.ReqNumericFluents           `at` $1 }
  | ':object-fluents'            { T.ReqObjectFluents            `at` $1 }
  | ':adl'                       { T.ReqAdl                      `at` $1 }
  | ':durative-actions'          { T.ReqDurativeActions          `at` $1 }
  | ':duration-inequalities'     { T.ReqDurationInequalities     `at` $1 }
  | ':continuous-effects'        { T.ReqContinuousEffects        `at` $1 }
  | ':derived-predicates'        { T.ReqDerivedPredicates        `at` $1 }
  | ':timed-initial-literals'    { T.ReqTimedInitialLiterals     `at` $1 }
  | ':preferences'               { T.ReqPreferences              `at` $1 }
  | ':constraints'               { T.ReqConstraints              `at` $1 }
  | ':action-costs'              { T.ReqActionCosts              `at` $1 }


-- Types -----------------------------------------------------------------------

types_def :: { T.TypedList (Located T.Name) }
  : '(:types' typed_list(name) ')' { $2               }
  | {- empty -}                    { T.UntypedList [] }

typed_list(p)
  : list1(p)        { T.UntypedList $1        }
  | list1(typed(p)) { T.TypedList (concat $1) }

typed(p)
  : list1(p) '-' type { [ T.Typed $3 v | v <- $1 ] }

type :: { Located T.Type }
  : name                      { T.Type `fmap` $1 }
  | '(either' list1(name) ')' { T.Either (map unLoc $2) `at` mconcat [$1,$3] }


-- Utilities -------------------------------------------------------------------

name :: { Located T.Name }
  : NAME { let Located loc (TSymbol (SName n)) = $1
            in Located loc n }

list(p)
  : list_body(p) { reverse $1 }
  | {- empty -}  { []         }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
  : list_body(p) p { $2 : $1 }
  | p              { [$1]    }
