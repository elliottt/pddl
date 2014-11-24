{
-- vim: ft=haskell

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Text.PDDL.Parser where

import           Text.PDDL.Core ( Parser, lexer, parseError )
import           Text.PDDL.Lexer ( Lexeme, Token(..), Keyword(..) )
import           Text.PDDL.Location ( Located(..), at, unLoc )
import qualified Text.PDDL.Types as T

import           Control.Applicative ( (<$) )
import           Data.Monoid ( mconcat )

import Debug.Trace

}

%token

  -- s-expressions
  '(' { Located $$ (TKeyword Klparen) }
  ')' { Located $$ (TKeyword Krparen) }
  '-' { Located $$ (TKeyword Kdash)   }

  -- keywords
  'define'        { Located $$ (TName "define")       }
  'domain'        { Located $$ (TName "domain")       }
  ':requirements' { Located $$ (TSym  "requirements") }
  ':types'        { Located $$ (TSym  "types")        }
  'either'        { Located $$ (TName "either")       }

  -- requirements
  ':strips'                    { Located $$ (TSym "strips")                    }
  ':typing'                    { Located $$ (TSym "typing")                    }
  ':negative-preconditions'    { Located $$ (TSym "negative-preconditions")    }
  ':disjunctive-preconditions' { Located $$ (TSym "disjunctive-preconditions") }
  ':equality'                  { Located $$ (TSym "equality")                  }
  ':existential-preconditions' { Located $$ (TSym "existential-preconditions") }
  ':universal-preconditions'   { Located $$ (TSym "universal-preconditions")   }
  ':conditional-effects'       { Located $$ (TSym "conditional-effects")       }
  ':fluents'                   { Located $$ (TSym "fluents")                   }
  ':numeric-fluents'           { Located $$ (TSym "numeric-fluents")           }
  ':object-fluents'            { Located $$ (TSym "object-fluents")            }
  ':adl'                       { Located $$ (TSym "adl")                       }
  ':durative-actions'          { Located $$ (TSym "durative-actions")          }
  ':duration-inequalities'     { Located $$ (TSym "duration-inequalities")     }
  ':continuous-effects'        { Located $$ (TSym "continuous-effects")        }
  ':derived-predicates'        { Located $$ (TSym "derived-predicates")        }
  ':timed-initial-literals'    { Located $$ (TSym "timed-initial-literals")    }
  ':preferences'               { Located $$ (TSym "preferences")               }
  ':constraints'               { Located $$ (TSym "constraints")               }
  ':action-costs'              { Located $$ (TSym "action-costs")              }

  NAME { $$ @ (Located _  (TName _))   }
  VAR  { $$ @ (Located _  (TVar _))    }


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
  : '(' 'define'
        '(' 'domain' name ')'
        require_def
        types_def
    ')'
     { T.Domain { T.dName     = $5
                , T.dRequires = $7
                , T.dTypes    = $8
                , T.dConsts   = T.UntypedList []
                , T.dPredSigs = []
                , T.dFuns     = T.UntypedList []
                , T.dCons     = []
                , T.dActions  = []
                }
     }


-- Requirements ----------------------------------------------------------------

require_def :: { [Located T.Requirement] }
  : '(' ':requirements' list1(require_key) ')' { $3 }
  | {- empty -}                                { [] }

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
  : '(' ':types' typed_list(name) ')' { $3               }
  | {- empty -}                       { T.UntypedList [] }

typed_list(p)
  : list1(p)        { T.UntypedList $1        }
  | list1(typed(p)) { T.TypedList (concat $1) }

typed(p)
  : list1(p) '-' type { [ T.Typed $3 v | v <- $1 ] }

type :: { Located T.Type }
  : name                         { T.Type `fmap` $1 }
  | '(' 'either' list1(name) ')' { T.Either (map unLoc $3) `at` mconcat [$1,$4] }


-- Utilities -------------------------------------------------------------------

name :: { Located T.Name }
  : NAME { let Located loc (TName n) = $1
            in Located loc n }

list(p)
  : list_body(p) { reverse $1 }
  | {- empty -}  { []         }

list1(p)
  : list_body(p) { reverse $1 }

list_body(p)
  : list_body(p) p { $2 : $1 }
  | p              { [$1]    }
