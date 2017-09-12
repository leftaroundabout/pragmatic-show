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
import Test.Tasty.QuickCheck ((==>))

import qualified Prelude
import Prelude hiding (Show(..))
import Text.Show.Pragmatic


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 [ testGroup "Graph structure of webs"
  [ testGroup "Re-Reading of exact types"
   [
   ]
  ]
 ]
