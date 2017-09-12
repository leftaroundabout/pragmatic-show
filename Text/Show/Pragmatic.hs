-- |
-- Module      : Text.Show.Pragmatic
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE CPP #-}

module Text.Show.Pragmatic where

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

#define StdShow(A)             \
instance Show (A) where {       \
  show = Prelude.show;           \
  showsPrec = Prelude.showsPrec;  \
  showList = Prelude.showList }

StdShow(Int)
StdShow(Integer)

instance Show Char where
  show c | c>'\31', c/='\'', c/='\\'
                      = '\'':c:"'"
         | otherwise  = Prelude.show c
  showList cs = ('"':) . flip (foldr showc) cs . ('"':)
   where showc '"' = ("\\\""++)
         showc '\\' = ("\\\\"++)
         showc c | c>'\31'    = (c:)
                 | otherwise  = case show c of
                     ('\'':q) -> case break (=='\'') q of
                       (r,"'") -> (r++)
         

instance (Show a) => Show [a] where
  showsPrec _ = showList
