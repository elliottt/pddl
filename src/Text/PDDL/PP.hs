{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.PDDL.PP (
    module Text.PDDL.PP
  , module Text.PrettyPrint.HughesPJ
  ) where

import qualified Data.Text as S
import           Text.PrettyPrint.HughesPJ


class PP a where
  pp :: a -> Doc

instance PP Int where
  pp = int

instance PP Double where
  pp = double

instance PP Doc where
  pp = id

instance PP Char where
  pp = char

instance PP S.Text where
  pp s = text (S.unpack s)
