-- |
-- Module      : Text.Show.SanePrecision
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Text.Show.SanePrecision where

import Prelude hiding (Show(..), shows)
import qualified Prelude

class Show a where
  {-# MINIMAL showsPrec | show #-}
  showsPrec :: Int -> a -> ShowS
  showsPrec _ x = (show x++)
  show :: a -> String
  show = (`shows`"")
  showList :: [a] -> ShowS
  showList [] = ("[]"++)
  showList (x:xs) = ('[':) . shows x . flip (foldr (\y -> (',':) . shows y)) xs . (']':)

shows :: Show a => a -> ShowS
shows = showsPrec 0
