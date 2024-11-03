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

import Data.Complex (Complex((:+)))
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
   , testProperty "[(Int,Bool)]" $ readBackEq ([]::[ [(Int,Bool)] ])
   , testProperty "([(Char,Int,Char)],[(Int,Char,Int,Char)])"
         $ readBackEq ([]::[ ([(Char,Int,Char)],[(Int,Char,Int,Char)]) ])
   ]
  , testGroup "Showing double-precision floats"
   [ floatTest 1 "1"
   , floatTest 10 "10"
   , floatTest 0.1 "0.1"
   , floatTest (1/3) "1/3"
   , floatTest 1e9 "1e9"
   , floatTest 1e90 "1e90"
   , floatTest pi "pi"
   , floatTest 32 "32"
   , floatTest (89.524 - 9.004) "80.52"
   , floatTest (0.3 + 0.3 + 0.3) "0.9"
   , testCase "FP-errors visible in standard instance" $ Prelude.show
               (0.3 + 0.3 + 0.3)@?="0.8999999999999999"
   , floatTest 325124 "325124"
   , floatTest 325124.512 "3.25124512e5"
   , floatTest 0.999999999 "0.999999999"
   , floatTest 1.52464e8 "1.52464e8"
   , floatTest (1.52464e8 + 1) "1.52464001e8"
   , floatTest (1.52464e8 + 1e-5) "1.52464e8"
   , floatTest 1e-48 "1e-48"
   , floatTest (1 - 1.52464e8) "-1.52463999e8"
   , floatTest 7e35 "7e35"
   , floatTest (1/0) "Infinity"
   , floatTest (sqrt $ -1) "NaN"
   , floatsTest [1,2,3] "[1,2,3]"
   , floatsTest (take 10 $ iterate (/16) 1)
       "[1,0.0625,3.90625e-3,2.44140625e-4,1.5258789e-5,9.53674e-7,5.9605e-8,3.725e-9,2.33e-10,1.5e-11]"
   , floatsTest [1, -sqrt 2, sqrt (-2)] "[1,-sqrt 2,NaN]"
   , floatsTest [pi**k | k <- [1, 4 .. 30]]
       "[pi,97,3020,93648,2.904e6,9.0032e7,2.791564e9,8.6556004e10,2.683779414e12,8.3214007069e13,2.58015652686e15]"
   ]
  , testGroup "Showing rational numbers"
   [ rationalTest 0 "0"
   , rationalTest 32 "32"
   , rationalTest (3/2) "3/2"
   , rationalTest (2/3) "2/3"
   , rationalTest (-268/19) "-268/19"
   , rationalTest (3/3) "1"
   ]
  , testGroup "Showing complex numbers"
   [ complexTest 1 "1"
   , complexTest (sqrt $ -1) "0:+1"
   , complexTest (exp $ 0:+pi) "-1"
   , complexTest (exp $ 0:+pi/4) "sqrt 2/2:+sqrt 2/2"
   , complexTest (exp $ 0:+5*pi/4) "(-sqrt 2/2):+(-sqrt 2/2)"
   ]
  , testGroup "Showing tuples and lists of tuples"
   [ floatTupleTest (1.001e-3, 1e40) "(1.001e-3,1e40)"
   , floatTuplesTest [ (1.001e-3,1e40),(3e-3,1.001) ]
                     "[(1.001e-3,1e40),(3e-3,1)]"
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

complexTest :: Complex Double -> String -> TestTree
complexTest n s = testCase s $ show n @?= s

floatsTest :: [Double] -> String -> TestTree
floatsTest n s = testCase s $ show n @?= s

floatTupleTest :: (Double,Double) -> String -> TestTree
floatTupleTest n s = testCase s $ show n @?= s

floatTuplesTest :: [(Double,Double)] -> String -> TestTree
floatTuplesTest n s = testCase s $ show n @?= s

rationalTest :: Rational -> String -> TestTree
rationalTest n s = testCase s $ show n @?= s
