{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.PDDL.PP (
    module Text.PDDL.PP
  , module Text.PrettyPrint.HughesPJ
  ) where

import Text.PrettyPrint.HughesPJ


class PP a where
  pp :: a -> Doc

instance PP Int where
  pp = int

instance PP Doc where
  pp = id
