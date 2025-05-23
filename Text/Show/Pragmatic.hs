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
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}

#include "HsBaseConfig.h"

module Text.Show.Pragmatic (
       -- * Replacement for the standard class
         Show(..), print
       -- * Utility (unstable)
       , ltdPrecShowsPrec
       , showsPrecWithSharedPrecision
       , ShowMagnitudeRangeLimited(..)
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
#if defined(HTYPE_TIMER_T)
                          , CTimer
#endif
                          , CKey, CId, CFsFilCnt, CFsBlkCnt
#if defined(HTYPE_CLOCKID_T)
                          , CClockId
#endif
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
import Data.Functor.Identity (Identity(..))



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
  {-# MINIMAL showsPrec | show | showEach #-}
  showsPrec :: Int -> a -> ShowS
  showsPrec _ x = (show x++)
  show :: a -> String
  show = (`shows`"")
  showEach :: Traversable t => Int     -- ^ Precedence to use for showing each individual element
                            -> t a     -- ^ List or other container of elements
                            -> t ShowS -- ^ Every element in the container shown, possibly with
                                       --   shared processing like trimmed insignificant decimals
  showEach = defaultShowEach
  showList :: [a] -> ShowS
  showList = defaultShowList

defaultAssembleListShow :: [ShowS] -> ShowS
defaultAssembleListShow [] = ("[]"++)
defaultAssembleListShow (x:xs) = ('[':) . x . flip (foldr (\y -> (',':) . y)) xs . (']':)

defaultShowList :: Show a => [a] -> ShowS
defaultShowList = defaultAssembleListShow . showEach 0

defaultShowEach :: (Show a, Traversable t) => Int -> t a -> t ShowS
defaultShowEach p = fmap $ showsPrec p

shows :: Show a => a -> ShowS
shows = runIdentity . showEach 0 . Identity

#define StdShow(A)             \
instance Show (A) where {       \
  show = Prelude.show;           \
  showsPrec = Prelude.showsPrec;  \
  showList = Prelude.showList }

StdShow(Bool)
StdShow(Int)
StdShow(Int8)
StdShow(Int16)
StdShow(Int32)
StdShow(Int64)
StdShow(Integer)

#if MIN_VERSION_base(4,8,0)
StdShow(Natural)
#endif

StdShow(Ordering)
StdShow(Word)
StdShow(Word8)
StdShow(Word16)
StdShow(Word32)
StdShow(Word64)

#if MIN_VERSION_base(4,9,0)
StdShow(CallStack)
#endif

#if MIN_VERSION_base(4,10,0)
StdShow(SomeTypeRep)
#endif

StdShow(())

#if MIN_VERSION_base(4,10,0)
StdShow(TyCon)
StdShow(Module)
#endif

#if MIN_VERSION_base(4,9,0)
StdShow(SrcLoc)
#endif

#if MIN_VERSION_base(3,0,0)
StdShow(SomeException)
#endif

StdShow(GeneralCategory)
StdShow(Number)
StdShow(Lexeme)

#if MIN_VERSION_base(4,7,0)
StdShow(Fingerprint)
#endif

StdShow(IOMode)
StdShow(IntPtr)
StdShow(WordPtr)
StdShow(CUIntMax)
StdShow(CIntMax)
StdShow(CUIntPtr)
StdShow(CIntPtr)
StdShow(CSUSeconds)
StdShow(CUSeconds)
StdShow(CTime)
StdShow(CClock)
StdShow(CSigAtomic)
StdShow(CWchar)
StdShow(CSize)
StdShow(CPtrdiff)

#if MIN_VERSION_base(4,10,0)
StdShow(CBool)
#endif

StdShow(CULLong)
StdShow(CLLong)
StdShow(CULong)
StdShow(CLong)
StdShow(CUInt)
StdShow(CInt)
StdShow(CUShort)
StdShow(CShort)
StdShow(CUChar)
StdShow(CSChar)
StdShow(CChar)

#if MIN_VERSION_base(4,10,0)
StdShow(SomeNat)
StdShow(SomeSymbol)
#endif

#if MIN_VERSION_base(4,9,0)
StdShow(DecidedStrictness)
StdShow(SourceStrictness)
StdShow(SourceUnpackedness)
StdShow(Associativity)
StdShow(GHC.Generics.Fixity)
#endif

StdShow(Any)
StdShow(All)

#if MIN_VERSION_base(4,0,0)
StdShow(ArithException)
StdShow(ErrorCall)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow(IOException)
#endif

StdShow(MaskingState)

#if MIN_VERSION_base(4,4,0)
StdShow(CodingProgress)
#endif

#if MIN_VERSION_base(4,3,0)
StdShow(TextEncoding)
#endif

StdShow(SeekMode)
StdShow(NewlineMode)
StdShow(Newline)
StdShow(BufferMode)

#if MIN_VERSION_base(4,1,0)
StdShow(Handle)
StdShow(IOErrorType)
#endif

StdShow(ExitCode)

#if MIN_VERSION_base(4,1,0)
StdShow(ArrayException)
StdShow(AsyncException)
#endif

#if MIN_VERSION_base(4,7,0)
StdShow(SomeAsyncException)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow(AssertionFailed)
#endif

#if MIN_VERSION_base(4,10,0)
StdShow(CompactionFailed)
#endif

#if MIN_VERSION_base(4,7,1)
StdShow(AllocationLimitExceeded)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow(Deadlock)
StdShow(BlockedIndefinitelyOnSTM)
StdShow(BlockedIndefinitelyOnMVar)
StdShow(CodingFailureMode)
#endif

StdShow(Fd)

#if MIN_VERSION_base(4,10,0)
#if defined(HTYPE_TIMER_T)
StdShow(CTimer)
#endif
StdShow(CKey)
StdShow(CId)
StdShow(CFsFilCnt)
StdShow(CFsBlkCnt)
#if defined(HTYPE_CLOCKID_T)
StdShow(CClockId)
#endif
StdShow(CBlkCnt)
StdShow(CBlkSize)
#endif
StdShow(CRLim)
StdShow(CTcflag)
StdShow(CSpeed)
StdShow(CCc)
StdShow(CUid)
StdShow(CNlink)
StdShow(CGid)
StdShow(CSsize)
StdShow(CPid)
StdShow(COff)
StdShow(CMode)
StdShow(CIno)
StdShow(CDev)

#if MIN_VERSION_base(4,8,1)
StdShow(Lifetime)
StdShow(Event)
#endif

#if MIN_VERSION_base(2,1,0)
StdShow(Dynamic)
#endif

StdShow(ThreadStatus)
StdShow(BlockReason)

#if MIN_VERSION_base(4,2,0)
StdShow(ThreadId)
#endif

#if MIN_VERSION_base(4,0,0)
StdShow(NestedAtomically)
StdShow(NonTermination)
#endif

#if MIN_VERSION_base(4,9,0)
StdShow(TypeError)
#endif

#if MIN_VERSION_base(4,0,0)
StdShow(NoMethodError)
StdShow(RecUpdError)
StdShow(RecConError)
StdShow(RecSelError)
StdShow(PatternMatchFail)
#endif

StdShow(FdKey)
#if MIN_VERSION_base(4,10,0)
StdShow(FileLockingNotSupported)
#endif

#if MIN_VERSION_base(4,1,0)
StdShow(HandlePosn)
#endif

#if MIN_VERSION_base(4,8,0)
StdShow(Version)
#endif

#if MIN_VERSION_base(4,10,0)
StdShow(RTSStats)
StdShow(ParFlags)
#endif
#if MIN_VERSION_base(4,9,0)
StdShow(RTSFlags)
StdShow(TickyFlags)
StdShow(TraceFlags)
StdShow(DoTrace)
StdShow(ProfFlags)
StdShow(DoHeapProfile)
StdShow(CCFlags)
StdShow(DoCostCentres)
StdShow(DebugFlags)
StdShow(MiscFlags)
StdShow(ConcFlags)
StdShow(GCFlags)
StdShow(GiveGCStats)
#endif

StdShow(Data.Data.Fixity)
StdShow(ConstrRep)
StdShow(DataRep)

#if MIN_VERSION_base(4,0,0)
StdShow(Constr)
#endif

StdShow(DataType)

#if MIN_VERSION_base(4,9,0)
StdShow(StaticPtrInfo)
#endif

#if MIN_VERSION_base(4,8,0)
StdShow(Void)
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
  showsPrecMagnitudeRangeLimited
          :: Int     -- ^ Precision of the data type
          -> Int     -- ^ Precedence of the showing context
          -> a       -- ^ Numerical value to show
          -> ShowS

instance Show Float where
  showsPrec = ltdPrecShowsPrec 7
  showEach = showsPrecWithSharedPrecision id 7
instance ShowMagnitudeRangeLimited Float where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

instance Show Double where
  showsPrec = ltdPrecShowsPrec 10
  showEach = showsPrecWithSharedPrecision id 10
instance ShowMagnitudeRangeLimited Double where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

instance Show CFloat where
  showsPrec = ltdPrecShowsPrec 5
  showEach = showsPrecWithSharedPrecision id 5
instance ShowMagnitudeRangeLimited CFloat where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

instance Show CDouble where
  showsPrec = ltdPrecShowsPrec 10
  showEach = showsPrecWithSharedPrecision id 10
instance ShowMagnitudeRangeLimited CDouble where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrec

showsPrecWithSharedPrecision :: (ShowMagnitudeRangeLimited n, RealFloat sn, Traversable list)
              => (n -> sn)   -- ^ Magnitude-function. Should be a norm.
              -> Int         -- ^ Precision of the type, in significant decimals. This will
                             --   be used to trim the length of all entries to match the
                             --   expected numerical uncertainty of the biggest one.
              -> Int         -- ^ Precedence of the enclosing context in which the values
                             --   are to be shown.
              -> list n      -- ^ Values to show
              -> list ShowS  -- ^ Individual values' string representation.
showsPrecWithSharedPrecision realise precision p vals
     = fmap (\val ->
              let uMagn = usableMagnitude $ realise val
              in showsPrecMagnitudeRangeLimited
                   (max 0 $ precision - floor (maxUMag - uMagn)) p val
            ) vals
 where usableMagnitude n
        | n<0            = usableMagnitude (-n)
        | n==n, 2*n>n    = logBase 10 n
        | otherwise      = -1/0
       maxUMag = maximum $ usableMagnitude . realise <$> vals

-- | @'ltdPrecShowsPrec' prcn@ displays floating-point values with a precision
--   of at least @prcn@ digits. That does not mean it will necessarily display
--   that many digits, rather it tries to always choose the shortest representation
--   with the required precision.
ltdPrecShowsPrec :: (RealFloat n)
          => Int     -- ^ Precision of the data type
          -> Int     -- ^ Precedence of the showing context
          -> n       -- ^ Numerical value to show
          -> ShowS
ltdPrecShowsPrec precision p n cont
    = minimumBy (comparing length)
        [ postProc $ ltdPrecShowsPrecDecimal precision p' (preProc n) ""
        | (preProc, p', postProc)
            <- [ (id, p, id) ]
             ++[ ( (/μ)
                 , 7, \s -> case s of
                        "1"   -> sμ ""
                        "(-1)"-> showParen (p>=6) (('-':) . sμ) ""
                        _     -> showParen (p>7) ((s++) . ('*':) . sμ) ""
                 )
               | (μ,sμ) <- (pi, ("pi"++))
                          :[ (pi / fromIntegral m, ("pi/"++) . shows m)
                           | m<-[2,3,4 :: Int] ]
                         ++[ (sqrt $ fromIntegral n, ("sqrt "++) . shows n)
                           | n<-[2,3,5 :: Int] ]
                         ++[ ( sqrt (fromIntegral n)/fromIntegral m
                             , ("sqrt "++) . shows n . ('/':) . shows m)
                           | n<-[2,3 :: Int]
                           , m<-[2,3 :: Int] ]
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
                  = shows (round n :: Int)
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

instance (Show a) => Show (Identity a) where
  showsPrec p (Identity x) = showParen (p>9) $ ("Identity "++) . showsPrec 11 x

instance (Show a) => Show (Maybe a) where
  showsPrec _ Nothing = ("Nothing"++)
  showsPrec p (Just x) = showParen (p>9) $ ("Just "++) . showsPrec 11 x

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

data TraverseFirst t b a where
  TraverseFirst :: { firstTraversed :: t (a,b) } -> TraverseFirst t b a
 deriving (Functor, Foldable, Traversable)

data TraverseSecond t a b where
  TraverseSecond :: { secondTraversed :: t (a,b) } -> TraverseSecond t a b
 deriving (Functor, Foldable, Traversable)

instance (Show a, Show b) => Show (a,b) where
  showEach _ = fmap (\(sa,sb) -> ('(':) . sa . (',':) . sb . (')':))
             .  firstTraversed . showEach 0 . TraverseFirst
             . secondTraversed . showEach 0 . TraverseSecond

data TraverseFirstOf3 t b c a where
  TraverseFirstOf3 :: { firstOf3Traversed :: t (a,b,c) } -> TraverseFirstOf3 t b c a
 deriving (Functor, Foldable, Traversable)

data TraverseSecondOf3 t a c b where
  TraverseSecondOf3 :: { secondOf3Traversed :: t (a,b,c) } -> TraverseSecondOf3 t a c b
 deriving (Functor, Foldable, Traversable)

data TraverseThird t a b c where
  TraverseThird :: { thirdTraversed :: t (a,b,c) } -> TraverseThird t a b c
 deriving (Functor, Foldable, Traversable)

instance (Show a, Show b, Show c) => Show (a,b,c) where
  showEach _ = fmap (\(sa,sb,sc) -> ('(':) . sa . (',':) . sb . (',':) . sc . (')':))
             .  firstOf3Traversed . showEach 0 . TraverseFirstOf3
             . secondOf3Traversed . showEach 0 . TraverseSecondOf3
             .     thirdTraversed . showEach 0 . TraverseThird

data TraverseFirstOf4 t b c d a where
  TraverseFirstOf4 :: { firstOf4Traversed :: t (a,b,c,d) } -> TraverseFirstOf4 t b c d a
 deriving (Functor, Foldable, Traversable)

data TraverseSecondOf4 t a c d b where
  TraverseSecondOf4 :: { secondOf4Traversed :: t (a,b,c,d) } -> TraverseSecondOf4 t a c d b
 deriving (Functor, Foldable, Traversable)

data TraverseThirdOf4 t a b d c where
  TraverseThirdOf4 :: { thirdOf4Traversed :: t (a,b,c,d) } -> TraverseThirdOf4 t a b d c
 deriving (Functor, Foldable, Traversable)

data TraverseFourth t a b c d where
  TraverseFourth :: { fourthTraversed :: t (a,b,c,d) } -> TraverseFourth t a b c d
 deriving (Functor, Foldable, Traversable)

instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d) where
  showEach _ = fmap (\(sa,sb,sc,sd) -> ('(':).sa.(',':).sb.(',':).sc.(',':).sd.(')':))
             .  firstOf4Traversed . showEach 0 . TraverseFirstOf4
             . secondOf4Traversed . showEach 0 . TraverseSecondOf4
             .  thirdOf4Traversed . showEach 0 . TraverseThirdOf4
             .    fourthTraversed . showEach 0 . TraverseFourth

instance (Integral i, Show i) => Show (Ratio i) where
  showsPrec p n
   | n<0                 = showParen (p>5) $ ('-':) . showsPrec 6 (-n)
   | denominator n == 1  = shows $ numerator n
   | otherwise           = showParen (p>6) $ shows (numerator n)
                                              . ('/':) . shows (denominator n)

instance Show (Complex Double) where
  showsPrec = ltdPrecShowsPrecComplex 10
  showEach = showsPrecWithSharedPrecision magnitude 10
instance Show (Complex Float) where
  showsPrec = ltdPrecShowsPrecComplex 7
  showEach = showsPrecWithSharedPrecision magnitude 7
instance (RealFloat a, Show (Complex a), ShowMagnitudeRangeLimited a)
    => ShowMagnitudeRangeLimited (Complex a) where
  showsPrecMagnitudeRangeLimited = ltdPrecShowsPrecComplex

ltdPrecShowsPrecComplex :: (RealFloat r, ShowMagnitudeRangeLimited r)
                           => Int -> Int -> Complex r -> ShowS
ltdPrecShowsPrecComplex precision p (r:+i)
 | abs r > abs i * 10^precision
    = ltdPrecShowsPrec precision p r
 | otherwise
    = case ($ "")<$>showsPrecWithSharedPrecision id precision 6 [r,i] of
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
