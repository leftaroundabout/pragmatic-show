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
  , testGroup "Showing double-precision floats"
   [ floatTest pi "3.14159265359"
   , floatTest 32 "32"
   , floatTest (89.524 - 9.004) "80.52"
   , floatTest (0.3 + 0.3 + 0.3) "0.9"
   , testCase "FP-errors visible in standard instance" $ Prelude.show
               (0.3 + 0.3 + 0.3)@?="0.8999999999999999"
   , floatTest 325124 "325124"
   , floatTest 325124.512 "3.25124512e5"
   , floatTest 1.52464e8 "1.52464e8"
   , floatTest (1.52464e8 + 1) "1.52464001e8"
   , floatTest (1.52464e8 + 1e-5) "1.52464e8"
   , floatTest 1e-48 "1e-48"
   , floatTest (1 - 1.52464e8) "-1.52463999e8"
   , floatTest 7e35 "7e35"
   , floatTest (1/0) "Infinity"
   , floatTest (sqrt $ -1) "NaN"
   ]
 ]

readBackEq :: (Show a, Read a, Eq a) => p a -> a -> Bool
readBackEq _ x = read (show x) == x

floatTest :: Double -> String -> TestTree
floatTest n s = testCase s $ show n @?= s
