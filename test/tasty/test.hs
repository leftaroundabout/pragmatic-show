-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE FlexibleContexts       #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>), testProperty)

import qualified Prelude
import Prelude hiding (Show(..))
import Text.Show.Pragmatic

import Data.VectorSpace


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
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
  , testGroup "Re-Reading of approximate types"
   [ testProperty "Double" $ readBackApproxEq ([]::[Double]) 1e-10
   , testProperty "Double with higher-than-supported precision"
          . QC.expectFailure
           $ readBackApproxEq ([]::[Double]) 1e-14
   , testProperty "Double²" $ readBackApproxEq ([]::[(Double,Double)]) 1e-10
   ]
  ]

-- | Check that showing and reading again yields the original value.
readBackEq :: (Show a, Read a, Eq a) => p a -> a -> Bool
readBackEq _ x = read (show x) == x

-- | Check that showing and reading again yields a value close to the original,
--   and that read.show is a projection (i.e. will only perhaps round once, but
--   if performed again not change anything further).
readBackApproxEq :: (Show a, Read a, InnerSpace a, RealFloat (Scalar a))
                       => p a -> Scalar a -> a -> Bool
readBackApproxEq _ ε x
  | m < 1e50   = magnitude (rs x ^-^ x) <= m*ε
                  && magnitude (rs (rs x) ^-^ rs x) == 0
  | otherwise  = True
 where m = magnitude x
       rs = read . show

floatTest :: Double -> String -> TestTree
floatTest n s = testCase s $ show n @?= s
