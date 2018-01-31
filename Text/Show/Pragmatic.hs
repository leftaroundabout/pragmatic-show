-- |
-- Module      : Text.Show.Pragmatic
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Show.Pragmatic (
       -- * Replacement for the standard class
         Show(..), print
       -- * Utility
       , ltdPrecShowsPrec
       ) where

import Prelude hiding (Show(..), shows, print)
import qualified Prelude
import Data.Foldable (toList)
import Data.List (intersperse, minimumBy)
import Data.Ord (comparing)

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Ratio
import Data.Complex (Complex((:+)), magnitude)
#if MIN_VERSION_base(4,8,0)
import Numeric.Natural (Natural)
#endif
#if MIN_VERSION_base(4,10,0)
import Type.Reflection (TyCon, SomeTypeRep, Module)
#endif
#if MIN_VERSION_base(4,9,0)
import GHC.Stack (SrcLoc, CallStack)
#endif
#if MIN_VERSION_base(3,0,0)
import Control.Exception.Base ( SomeException, ArithException, ErrorCall, IOException
                              , MaskingState
                              , ArrayException, AsyncException
#if MIN_VERSION_base(4,7,0)
                              , SomeAsyncException
#endif
                              , AssertionFailed
#if MIN_VERSION_base(4,10,0)
                              , CompactionFailed
#endif
#if MIN_VERSION_base(4,7,1)
                              , AllocationLimitExceeded
#endif
                              , Deadlock, BlockedIndefinitelyOnSTM
                              , BlockedIndefinitelyOnMVar
                              , NestedAtomically, NonTermination
#if MIN_VERSION_base(4,9,0)
                              , TypeError
#endif
                              , NoMethodError
                              , RecUpdError, RecConError, RecSelError, PatternMatchFail
                              )
#endif
import Data.Char (GeneralCategory)
import Text.Read.Lex (Number, Lexeme)
#if MIN_VERSION_base(4,7,0)
import GHC.Fingerprint.Type (Fingerprint)
#endif
import System.IO (IOMode)
import System.IO.Error (IOErrorType)
import System.Exit (ExitCode)
import Foreign.Ptr (IntPtr, WordPtr)
import Foreign.C.Types ( CUIntMax, CIntMax, CUIntPtr, CIntPtr
                       , CSUSeconds, CUSeconds, CTime, CClock
                       , CSigAtomic, CWchar, CSize, CPtrdiff
                       , CDouble, CFloat
#if MIN_VERSION_base(4,10,0)
                       , CBool
#endif
                       , CULLong, CLLong, CULong, CLong, CUInt, CInt, CUShort, CShort
                       , CUChar, CSChar, CChar
                       )
#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (SomeNat)
import GHC.TypeLits (SomeSymbol)
#endif
#if MIN_VERSION_base(4,9,0)
import GHC.Generics ( DecidedStrictness, SourceStrictness, SourceUnpackedness
                    , Associativity, Fixity )
#endif
import Data.Monoid (Any, All)
#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding.Types (CodingProgress, TextEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode)
#endif
import GHC.IO.Device (SeekMode)
import GHC.IO.Handle (NewlineMode, Newline, BufferMode, Handle, HandlePosn)
#if MIN_VERSION_base(4,10,0)
import GHC.IO.Handle.Lock (FileLockingNotSupported)
#endif
#if MIN_VERSION_base(4,9,0)
import GHC.StaticPtr (StaticPtrInfo)
#endif
import System.Posix.Types ( Fd
#if MIN_VERSION_base(4,10,0)
                          , CTimer, CKey, CId, CFsFilCnt, CFsBlkCnt, CClockId
                          , CBlkCnt, CBlkSize
#endif
                          , CRLim, CTcflag, CSpeed, CCc, CUid
                          , CNlink, CGid, CSsize, CPid, COff, CMode, CIno, CDev )
#if MIN_VERSION_base(4,8,1)
import GHC.Event (Lifetime, Event, FdKey)
#endif
#if MIN_VERSION_base(2,1,0)
import Data.Dynamic (Dynamic)
#endif
import GHC.Conc (ThreadStatus, BlockReason)
import Control.Concurrent (ThreadId)
#if MIN_VERSION_base(4,8,0)
import Data.Version (Version)
#endif
#if MIN_VERSION_base(4,5,0)
import Data.Version (Version)
#endif
#if MIN_VERSION_base(4,10,0)
import GHC.Stats (RTSStats)
#endif
#if MIN_VERSION_base(4,9,0)
import GHC.RTS.Flags ( RTSFlags
#if MIN_VERSION_base(4,10,0)
                     , ParFlags
#endif
                     , TickyFlags, TraceFlags, DoTrace, ProfFlags
                     , DoHeapProfile, CCFlags, DoCostCentres, DebugFlags, MiscFlags
                     , ConcFlags, GCFlags, GiveGCStats )
#endif
import Data.Data ( Fixity, ConstrRep, DataRep
#if MIN_VERSION_base(4,0,0)
                 , Constr
#endif
                 , DataType )
#if MIN_VERSION_base(4,8,0)
import Data.Void
#endif

import qualified Data.Set as Set
import qualified Data.IntSet as ℤSet
import qualified Data.Map as Map
import qualified Data.IntMap as ℤMap
import qualified Data.Sequence as Seq
import qualified Data.Tree as Tree



-- | A drop-in replacement for 'Prelude.Show'. The behaviour is mostly the same:
--   the result of 'show' should be valid Haskell code, and 'read'ing back such a
--   value should give the original value – but, unlike in 'Prelude.Show', we don't
--   require this in an /exact/ sense, i.e. @'read' ('show' x) == x@ is not necessarily
--   fulfilled.
--   
--   Notably for floating-point values, we allow a slight deviation if
--   it considerably shortens the shown representation: for example,
--   @0.90000004 :: Float@, which can easily come up as
--   the result of a computation which should in principle be exactly @0.9@, is shown
--   as @0.9@ instead. We do however /not/ commit to any particular fixed precision;
--   it depends on the type and the order of magnitude which amount of rounding is
--   appropriate. See <https://github.com/leftaroundabout/pragmatic-show/blob/master/test/tasty/test.hs the test suite> for some examples.
class Show a where
  {-# MINIMAL showsPrec | show #-}
  showsPrec :: Int -> a -> ShowS
  showsPrec _ x = (show x++)
  show :: a -> String
  show = (`shows`"")
  showList :: [a] -> ShowS
  showList = defaultShowList

defaultShowList :: Show a => [a] -> ShowS
defaultShowList [] = ("[]"++)
defaultShowList (x:xs) = ('[':) . shows x . flip (foldr (\y -> (',':) . shows y)) xs . (']':)

shows :: Show a => a -> ShowS
shows = showsPrec 0

#define StdShow(A)             \
instance Show (A) where {       \
  show = Prelude.show;           \
  showsPrec = Prelude.showsPrec;  \
  showList = Prelude.showList }

StdShow (Bool)
StdShow (Int)
StdShow (Int8)
StdShow (Int16)
StdShow (Int32)
StdShow (Int64)
StdShow (Integer)

#if MIN_VERSION_base(4,8,0)
StdShow (Natural)
#endif

StdShow (Ordering)
StdShow (Word)
StdShow (Word8)
StdShow (Word16)
StdShow (Word32)
StdShow (Word64)

#if MIN_VERSION_base(4,9,0)
StdShow (CallStack)
#endif

#if MIN_VERSION_base(4,10,0)
StdShow (SomeTypeRep)
#endif

StdShow (())

#if MIN_VERSION_base(4,10,0)
StdShow (TyCon)
StdShow (Module)
#endif

#if MIN_VERSION_base(4,9,0)
StdShow (SrcLoc)
#endif

#if MIN_VERSION_base(3,0,0)
StdShow (SomeException)
#endif

StdShow (GeneralCategory)
StdShow (Number)
StdShow (Lexeme)

#if MIN_VERSION_base(4,7,0)
StdShow (Fingerprint)
#endif

StdShow (IOMode)
StdShow (IntPtr)
StdShow (WordPtr)
StdShow (CUIntMax)
StdShow (CIntMax)
StdShow (CUIntPtr)
StdShow (CIntPtr)
StdShow (CSUSeconds)
StdShow (CUSeconds)
StdShow (CTime)
StdShow (CClock)
StdShow (CSigAtomic)
StdShow (CWchar)
StdShow (CSize)
StdShow (CPtrdiff)

#if MIN_VERSION_base(4,10,0)
StdShow (CBool)
#endif

StdShow (CULLong)
StdShow (CLLong)
StdShow (CULong)
StdShow (CLong)
StdShow (CUInt)
StdShow (CInt)
StdShow (CUShort)
StdShow (CShort)
StdShow (CUChar)
StdShow (CSChar)
StdShow (CChar)

#if MIN_VERSION_base(4,10,0)
StdShow (SomeNat)
StdShow (SomeSymbol)
#endif

#if MIN_VERSION_base(4,9,0)
StdShow (DecidedStrictness)
StdShow (SourceStrictness)
StdShow (SourceUnpackedness)
StdShow (Associativity)
StdShow (GHC.Generics.Fixity)
#endif

StdShow (Any)
StdShow (All)

#if MIN_VERSION_base(4,0,0)
StdShow (ArithException)
StdShow (ErrorCall)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow (IOException)
#endif

StdShow (MaskingState)

#if MIN_VERSION_base(4,4,0)
StdShow (CodingProgress)
#endif

#if MIN_VERSION_base(4,3,0)
StdShow (TextEncoding)
#endif

StdShow (SeekMode)
StdShow (NewlineMode)
StdShow (Newline)
StdShow (BufferMode)

#if MIN_VERSION_base(4,1,0)
StdShow (Handle)
StdShow (IOErrorType)
#endif

StdShow (ExitCode)

#if MIN_VERSION_base(4,1,0)
StdShow (ArrayException)
StdShow (AsyncException)
#endif

#if MIN_VERSION_base(4,7,0)
StdShow (SomeAsyncException)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow (AssertionFailed)
#endif

#if MIN_VERSION_base(4,10,0)
StdShow (CompactionFailed)
#endif

#if MIN_VERSION_base(4,7,1)
StdShow (AllocationLimitExceeded)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow (Deadlock)
StdShow (BlockedIndefinitelyOnSTM)
StdShow (BlockedIndefinitelyOnMVar)
StdShow (CodingFailureMode)
#endif

StdShow (Fd)

#if MIN_VERSION_base(4,10,0)
StdShow (CTimer)
StdShow (CKey)
StdShow (CId)
StdShow (CFsFilCnt)
StdShow (CFsBlkCnt)
StdShow (CClockId)
StdShow (CBlkCnt)
StdShow (CBlkSize)
#endif
StdShow (CRLim)
StdShow (CTcflag)
StdShow (CSpeed)
StdShow (CCc)
StdShow (CUid)
StdShow (CNlink)
StdShow (CGid)
StdShow (CSsize)
StdShow (CPid)
StdShow (COff)
StdShow (CMode)
StdShow (CIno)
StdShow (CDev)

#if MIN_VERSION_base(4,8,1)
StdShow (Lifetime)
StdShow (Event)
#endif

#if MIN_VERSION_base(2,1,0)
StdShow (Dynamic)
#endif

StdShow (ThreadStatus)
StdShow (BlockReason)

#if MIN_VERSION_base(4,2,0)
StdShow (ThreadId)
#endif

#if MIN_VERSION_base(4,0,0)
StdShow (NestedAtomically)
StdShow (NonTermination)
#endif

#if MIN_VERSION_base(4,9,0)
StdShow (TypeError)
#endif

#if MIN_VERSION_base(4,0,0)
StdShow (NoMethodError)
StdShow (RecUpdError)
StdShow (RecConError)
StdShow (RecSelError)
StdShow (PatternMatchFail)
#endif

StdShow (FdKey)
#if MIN_VERSION_base(4,10,0)
StdShow (FileLockingNotSupported)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow (HandlePosn)
#endif

#if MIN_VERSION_base(4,8,0)
StdShow (Version)
#endif

#if MIN_VERSION_base(4,10,0)
StdShow (RTSStats)
StdShow (ParFlags)
#endif
#if MIN_VERSION_base(4,9,0)
StdShow (RTSFlags)
StdShow (TickyFlags)
StdShow (TraceFlags)
StdShow (DoTrace)
StdShow (ProfFlags)
StdShow (DoHeapProfile)
StdShow (CCFlags)
StdShow (DoCostCentres)
StdShow (DebugFlags)
StdShow (MiscFlags)
StdShow (ConcFlags)
StdShow (GCFlags)
StdShow (GiveGCStats)
#endif

StdShow (Data.Data.Fixity)
StdShow (ConstrRep)
StdShow (DataRep)

#if MIN_VERSION_base(4,0,0)
StdShow (Constr)
#endif

StdShow (DataType)

#if MIN_VERSION_base(4,9,0)
StdShow (StaticPtrInfo)
#endif

#if MIN_VERSION_base(4,8,0)
StdShow (Void)
#endif


instance Show Char where
  show c | c>'\31', c/='\'', c/='\\'
                      = '\'':c:"'"
         | otherwise  = Prelude.show c
  showList cs = ('"':) . flip (foldr showc) cs . ('"':)
   where showc '"' = ("\\\""++)
         showc '\\' = ("\\\\"++)
         showc '\SO' = ("\\SO\\&"++)  -- prevent problem with "\SO\&H"≈[14,72] getting
                                      -- shown as "\SOH"≈[2]. (Thanks, QuickCheck!)
         showc c | c>'\31'    = (c:)
                 | otherwise  = case show c of
                     ('\'':q) -> case break (=='\'') q of
                       (r,"'") -> (r++)


class Show a => ShowMagnitudeRangeLimited a where
  showsPrecMagnitudeRangeLimited :: Int -> Int -> a -> ShowS

instance Show Float where
  showsPrec = ltdPrecShowsPrec 7
  showList = ltdPrecShowList id 7
instance ShowMagnitudeRangeLimited Float where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

instance Show Double where
  showsPrec = ltdPrecShowsPrec 10
  showList = ltdPrecShowList id 10
instance ShowMagnitudeRangeLimited Double where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

instance Show CFloat where
  showsPrec = ltdPrecShowsPrec 5
  showList = ltdPrecShowList id 5
instance ShowMagnitudeRangeLimited CFloat where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

instance Show CDouble where
  showsPrec = ltdPrecShowsPrec 10
  showList = ltdPrecShowList id 10
instance ShowMagnitudeRangeLimited CDouble where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

ltdPrecShowList :: (ShowMagnitudeRangeLimited n, RealFloat sn)
                   => (n -> sn) -> Int -> [n] -> ShowS
ltdPrecShowList realise precision vals
          = ('[':) . flip (foldr id)
                          (intersperse (',':)
                             $ ltdPrecShowsPrec_par realise precision 0 vals)
                   . (']':)

ltdPrecShowsPrec_par :: (ShowMagnitudeRangeLimited n, RealFloat sn)
              => (n -> sn) -> Int -> Int -> [n] -> [ShowS]
ltdPrecShowsPrec_par realise precision p vals
          = [ showsPrecMagnitudeRangeLimited
                (max 0 $ precision - floor (maxUMag - uMagn)) p val
            | (val,uMagn) <- zip vals usableMagnitudes ]
 where usableMagnitude n
        | n<0            = usableMagnitude (-n)
        | n==n, 2*n>n    = logBase 10 n
        | otherwise      = -1/0
       usableMagnitudes = (usableMagnitude . realise) <$> vals
       maxUMag = maximum usableMagnitudes

-- | @'ltdPrecShowsPrec' prcn@ displays floating-point values with a precision
--   of at least @prcn@ digits. That does not mean it will necessarily display
--   that many digits, rather it tries to always choose the shortest representation
--   with the required precision.
ltdPrecShowsPrec :: (RealFloat n) => Int -> Int -> n -> ShowS
ltdPrecShowsPrec precision p n cont
    = minimumBy (comparing length)
        [ postProc $ ltdPrecShowsPrecDecimal precision p' (preProc n) ""
        | (preProc, p', postProc)
            <- [ (id, p, id) ]
             ++[ ( (/μ)
                 , 7, \s -> case s of
                        "1"   -> sμ ""
                        "(-1)"-> showParen (p>7) (('-':) . sμ) ""
                        _     -> showParen (p>7) ((s++) . ('*':) . sμ) ""
                 )
               | (μ,sμ) <- (pi, ("pi"++))
                         : [ (sqrt $ fromIntegral n, ("sqrt "++) . shows n)
                           | n<-[2,3,5 :: Int] ]
               ]
             ++[ ( (*fromIntegral n)
                 , 7, \s -> showParen (p>7) ((s++) . ('/':) . shows n) "" )
               | n<-[3,7,9 :: Int] ]
        ] ++ cont

ltdPrecShowsPrecDecimal :: (RealFloat n) => Int -> Int -> n -> ShowS
ltdPrecShowsPrecDecimal _ _ 0 = ("0"++)
ltdPrecShowsPrecDecimal precision p n
    | not (n==n)  = ("NaN"++)
    | n<0         = showParen (p>5)
                      $ ('-':) . ltdPrecShowsPrecDecimal precision 0 (negate n)
    | n==n*2      = ("Infinity"++)
    | e₁₀<7 && lrDigs <= e₁₀
                  = (rDigits++) . (replicate (e₁₀-lrDigs) '0' ++)
    | e₁₀>0 && e₁₀<3
                  = (take e₁₀ rDigits++) . ('.':) . (drop e₁₀ rDigits++)
    | e₁₀> -2 && e₁₀<=0
                  = ("0."++) . (replicate (negate e₁₀) '0'++) . (rDigits++)
    | [hd] <- rDigits
                  = (hd:) . ("e"++) . shows (e₁₀-1)
    | (hd:qd@(_:_)) <- rDigits
                  = (hd:) . ('.':) . (qd++) . ("e"++) . shows (e₁₀-1)
   where (e₁₀,m₁₀Approx) = correctPrecision . ceiling $ logBase 10 n
          where correctPrecision e
                 = case show (round $ n * 10^^(precision+2 - e) :: Int) of
                     digits | length digits <= precision+2  -> (e,digits)
                            | otherwise                     -> correctPrecision $ e+1
         (rApprZeroes, rDigits') = break (>'0') . reverse $ m₁₀Approx
         rDigits = reverse rDigits'
         lrDigs = length rDigits

instance (Show a) => Show [a] where
  showsPrec _ = showList

instance (Show a, Ord a) => Show (Seq.Seq a) where
  showsPrec _ = defaultShowList . toList
instance (Show a, Ord a) => Show (Set.Set a) where
  showsPrec _ = defaultShowList . Set.toList
instance Show ℤSet.IntSet where
  showsPrec _ = defaultShowList . ℤSet.toList
instance (Show a, Ord a, Show b) => Show (Map.Map a b) where
  showsPrec _ = defaultShowList . Map.toList
instance (Show b) => Show (ℤMap.IntMap b) where
  showsPrec _ = defaultShowList . ℤMap.toList
instance (Show a) => Show (Tree.Tree a) where
  showsPrec p (Tree.Node a st) = showParen (p>9)
                $ ("Node "++) . showsPrec 11 a . (' ':) . shows st

instance (Show a, Show b) => Show (a,b) where
  showsPrec _ (a,b) = ('(':) . shows a . (',':) . shows b . (')':)
instance (Show a, Show b, Show c) => Show (a,b,c) where
  showsPrec _ (a,b,c) = ('(':) . shows a . (',':) . shows b . (',':) . shows c . (')':)
instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d) where
  showsPrec _ (a,b,c,d) = ('(':)
           . shows a . (',':) . shows b . (',':) . shows c . (',':) . shows d
                        . (')':)

instance (Integral i, Show i) => Show (Ratio i) where
  showsPrec p n
   | n<0                 = showParen (p>5) $ ('-':) . showsPrec 6 (-n)
   | denominator n == 1  = shows $ numerator n
   | otherwise           = showParen (p>6) $ shows (numerator n)
                                              . ('/':) . shows (denominator n)

instance Show (Complex Double) where
  showsPrec = ltdPrecShowsPrecComplex 10
  showList = ltdPrecShowList magnitude 10
instance Show (Complex Float) where
  showsPrec = ltdPrecShowsPrecComplex 7
  showList = ltdPrecShowList magnitude 7
instance (RealFloat a, Show (Complex a), ShowMagnitudeRangeLimited a)
    => ShowMagnitudeRangeLimited (Complex a) where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrecComplex

ltdPrecShowsPrecComplex :: (RealFloat r, ShowMagnitudeRangeLimited r)
                           => Int -> Int -> Complex r -> ShowS
ltdPrecShowsPrecComplex precision p (r:+i)
 | abs r > abs i * 10^precision
    = ltdPrecShowsPrec precision p r
 | otherwise
    = case ($"")<$>ltdPrecShowsPrec_par id precision 6 [r,i] of
           [sr,"0"] -> showParen (p>7) $ (sr++)
           [sr,si] -> showParen (p>6) $ (sr++) . (":+"++) . (si++)

-- | Drop-in for the standard screen-displaying function. This is useful as a GHCi
--   evaluation action; invoke with
-- @
-- $ ghci -interactive-print=Text.Show.Pragmatic.print
-- @
--   to get more concise output from the REPL.
print :: Show a => a -> IO ()
print = putStrLn . show
