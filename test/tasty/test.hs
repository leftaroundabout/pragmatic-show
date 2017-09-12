-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>), testProperty)

import qualified Prelude
import Prelude hiding (Show(..))
import Text.Show.Pragmatic


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 [ testGroup "Graph structure of webs"
  [ testGroup "Re-Reading of exact types"
   [ testProperty "Char" $ readBackEq ([]::[Char])
   , testProperty "Int" $ readBackEq ([]::[Int])
   , testProperty "[[Int]]" $ readBackEq ([]::[ [[Int]] ])
   , testProperty "String" $ readBackEq ([]::[String])
   , testProperty "String vs standard instance"
      . QC.expectFailure -- we don't escape non-ASCII Unicode chars, unlike 'Prelude.show'.
       $ \s -> show s == Prelude.show (s :: String)
   , testProperty "(Int,Integer)" $ readBackEq ([]::[(Int,Integer)])
   , testProperty "(Int,Integer,(Char,[Int]),String)"
         $ readBackEq ([]::[(Int,Integer,(Char,[Int]),String)])
   ]
  ]
 ]

readBackEq :: (Show a, Read a, Eq a) => p a -> a -> Bool
readBackEq _ x = read (show x) == x
