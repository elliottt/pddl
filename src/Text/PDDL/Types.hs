{-# LANGUAGE DeriveFunctor #-}

module Text.PDDL.Types where

import           Text.PDDL.Location

import qualified Data.Text as S


data PDDL = PDDLProblem Problem
          | PDDLDomain Domain
            deriving (Show)

type Name = S.Text
type Pred = Name

type Var  = S.Text

data Domain = Domain { dName     :: Located Name
                     , dRequires :: [Located Requirement]
                     , dTypes    :: TypedList (Located Name)
                     , dConsts   :: TypedList Name
                     , dPredSigs :: [PredSig]
                     , dFuns     :: TypedList Function
                     , dCons     :: [Con]
                     , dActions  :: [Action]
                     } deriving (Show)

data Problem = Problem { pName     :: !Name
                       , pDomain   :: !Name
                       , pRequires :: [Requirement]
                       } deriving (Show)

data Requirement = ReqStrips
                 | ReqTyping
                 | ReqNegativePreconditions
                 | ReqDisjunctivePreconditions
                 | ReqEquality
                 | ReqExistentialPreconditions
                 | ReqUniversalPreconditions
                 | ReqQuantifiedPreconditions
                 | ReqConditionalEffects
                 | ReqFluents
                 | ReqNumericFluents
                 | ReqObjectFluents
                 | ReqAdl
                 | ReqDurativeActions
                 | ReqDurationInequalities
                 | ReqContinuousEffects
                 | ReqDerivedPredicates
                 | ReqTimedInitialLiterals
                 | ReqPreferences
                 | ReqConstraints
                 | ReqActionCosts
                   deriving (Show)

data TypedList a = UntypedList [a]
                 | TypedList [Typed a]
                   deriving (Show,Functor)

data Typed a = Typed { tType :: Located Type
                     , tVal  :: a
                     } deriving (Show,Functor)

data Type = Type Name
          | Either [Name]
            deriving (Show)

data PredSig = PredSig { psName :: Name
                       , psArgs :: TypedList Name
                       } deriving (Show)

data Function = Function { funName :: Name
                         , funArgs :: TypedList Name
                         } deriving (Show)

data Term = TVar Var
          | TName Name
            deriving (Show)

data GoalDesc = GDAnd [GoalDesc]
              | GDOr [GoalDesc]
              | GDNot GoalDesc
              | GDImply GoalDesc GoalDesc
              | GDExists (TypedList Var) GoalDesc
              | GDForall (TypedList Var) GoalDesc
              | GDLit (Literal Term)
                deriving (Show)

data Con = CForall (TypedList Name) [Con]
         | CAtEnd GoalDesc
         | CAlways GoalDesc
         | CSometime GoalDesc
         | CWithin Int GoalDesc
         | CAtMostOnce GoalDesc
         | CSometimeAfter GoalDesc GoalDesc
         | CSometimeBefore GoalDesc GoalDesc
         | CAlwaysWithin Int GoalDesc GoalDesc
         | CHoldDuring Int Int GoalDesc
         | CHoldAfter Int GoalDesc
           deriving (Show)

data Literal a = Literal Bool (Formula a)
                 deriving (Show)

data Formula a = FPred Pred [a]
               | FEq a a
                 deriving (Show)

data Action = Action
              deriving (Show)
