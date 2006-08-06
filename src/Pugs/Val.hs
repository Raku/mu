{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
{-! global : YAML_Pos, Perl6Class, MooseClass !-}
{-|
    Perl 6 Values.

>   There beryl, pearl, and opal pale,
>   And metal wrought like fishes' mail,
>   Buckler and corslet, axe and sword,
>   And shining spears were laid in hoard...
-}
module Pugs.Val (
    Val(..), ValUndef, ValNative, Id,
) where
import Pugs.Internals
import GHC.Exts
import Data.Generics.Basics
import qualified Data.ByteString as Buf

{-|

This module contains the definition of the five variants for a Perl 6 value.
However, the actual constructors for each variant are abstract, and this module
does not provide concrete data type definitions beyond those five.

-}

-- | 'Val' represents what an unconstrained scalar container can hold.
data Val
    = VUndef  !ValUndef   -- ^ Values that are false on .defined      (ValId = 0)
    | VNative !ValNative  -- ^ Values that can fit into an UArray     (ValId = impl.dep.)
    | forall a. Pure a => VPure !a  -- ^ Values that are immutable    (ValId = pureId)
    | forall a. Mut a  => VMut  !a  -- ^ In-memory mutable structures (ValId = memory addr)
    | forall a. Ext a  => VExt  !a  -- ^ Input/Ouput handles          (ValId = memory addr)
    deriving (Typeable)

instance Value Val where
    valId VUndef{}    = NBit False
    valId (VNative x) = nativeId x
    valId (VPure x)   = valId x
    valId (VMut x)    = valId x
    valId (VExt x)    = valId x

-- | 'Id' is an unique ID that distinguishes two @Val@s of the same type from each other.
type Id = ValNative

--------------------------------------------------------------------------------------

-- | L<S06/"Undefined types">
data ValUndef
    = UUndef                        -- ^ "my $x"
    | UWhatever                     -- ^ "my $x = *"
    | UFailure  { f_err  :: !Id }   -- ^ "my $x = fail 'oops'"
    | UProto    { p_meta :: !Id }   -- ^ "my $x = Dog"
    deriving (Show, Eq, Ord, Data, Typeable)

--------------------------------------------------------------------------------------
-- | Unboxed values.
data ValNative
    = NBit      !NativeBit      -- ^ 0
    | NInt      !NativeInt      -- ^ -3
    | NUint     !NativeUint     -- ^ 7
    | NBuf      !NativeBuf      -- ^ (a raw chunk of ints or uints)
    | NNum      !NativeNum      -- ^ 4.2
    | NComplex  !NativeComplex  -- ^ (45 - 9i)
    deriving (Show, Eq, Ord, Data, Typeable)

nativeId :: ValNative -> Id
nativeId (NBit True)            = NInt (-1)
nativeId (NBit False)           = NBit True
nativeId (NInt 0)               = NBit True
nativeId (NUint 0)              = NBit True
nativeId (NNum 0)               = NBit True
nativeId (NBuf x) | Buf.null x  = NBit True
nativeId x                      = x

type NativeBit      = Bool
type NativeInt      = Int
type NativeUint     = Word
type NativeBuf      = ByteString
type NativeNum      = Float
type NativeComplex  = () -- Complex NativeNum 

--------------------------------------------------------------------------------------
class Value a => Pure a where
    pureId :: a -> Id
    pureId = error "Pure values can't use memory address for Id - define pureId first"

class Value a => Mut a where
class Value a => Ext a where

class (Show a, Eq a, Ord a, Data a, Typeable a) => Value a where
    valId :: a -> Id
    valId = NUint . unsafeCoerce#

instance Pure a => Value a where
    valId = pureId

dynEq :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
dynEq x y = case cast y of
    Just y' -> x == y'
    Nothing -> False

dynCompare :: (Typeable a, Typeable b, Ord a) => a -> b -> Ordering
dynCompare x y = case cast y of
    Just y' -> compare x y'
    Nothing -> compare (show $ typeOf x) (show $ typeOf y)

{-
    = PBit       !PureBit
    | PInt       !PureInt
    | PStr       !PureStr
    | PNum       !PureNum
    | PComplex   !PureComplex
    | PBool      !PureBool
    | PException !PureException -- XXX
    | PCode      !PureCode
    | PBlock     !PureCode -- XXX: or more primitive type?
    | PList      !PureList
    | PSeq       !PureSeq
    | PRange     !PureRange
    | PSet       !PureSet
    | PJunc      !PureJunc
    | PPair      !PurePair
    | PMap       !PureMap
    | PSig       !PureSig
    | PCap       !PureCap 
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-}
type PureBit        = Bool
type PureStr        = ByteString    -- XXX *very* bogus
type PureBool       = Bool
type PureException  = ()            -- XXX *very* bogus
type PureCode       = ()            -- XXX *very* bogus
type PureSig        = ()
type PureCap        = ()
type PureSet        = Set Val
type PureSeq        = Seq Val
type PureList       = Seq (Either PureSeq PureRange) -- XXX - *very bogus*
type PureComplex    = ()
type PureInt        = ()
type PureNum        = ()
type PureRange      = ()
type PureJunc       = ()
type PurePair       = ()
type PureMap        = ()

{-
--------------------------------------------------------------------------------------
-- | L<S06/"Mutable types"> minus IO types
--   Computations on these types take place in the STM monad.
data ValMut
    = MScalar    !MutScalar
    | MArray     !MutArray
    | MHash      !MutHash
    | MBuf       !MutBuf
    | MRoutine   !MutRoutine
    | MSub       !MutRoutine -- ?
    | MMethod    !MutRoutine -- ?
    | MSubmethod !MutRoutine -- ?
    | MMacro     -- ???
    | MRegex     !MutVRule -- XXX: maybe move to pure
    | MMatch     !MutVMatch
    | MPackage   !MutPackage
    | MModule    !MutModule
    | MClass     !MutClass
    | MRole      !MutRole
    | MGrammar   !MutGrammar
    | MObject    !MutObject  -- ? or ObjectId?
    | MForeign   !MutDynamic -- ...?
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
-}

--------------------------------------------------------------------------------------
{-
-- | Obviously side-effectual types such as file handles.
--   Computations on these types must take place in the IO monad.
data ValExt
    = IFile     !ExtFile     -- ^ File handle
    | ISocket   !ExtSocket   -- ^ Socket handle
    | IThread   !ExtThread   -- ^ Thread handle
    | IProcess  !ExtProcess  -- ^ Process handle
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
-}

type ExtFile         = ()
type ExtSocket       = ()
type ExtThread       = ()
type ExtProcess      = ()


{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Show Val where
    showsPrec d (VUndef aa) = showParen (d >= 10)
              (showString "VUndef" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (VNative aa) = showParen (d >= 10)
              (showString "VNative" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (VPure aa) = showParen (d >= 10)
              (showString "VPure" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (VMut aa) = showParen (d >= 10)
              (showString "VMut" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (VExt aa) = showParen (d >= 10)
              (showString "VExt" . showChar ' ' . showsPrec 10 aa)

instance Eq Val where
    (VUndef aa)  == (VUndef aa')    = aa == aa'
    (VNative aa) == (VNative aa')   = aa == aa'
    (VPure aa)   == (VPure aa')     = dynEq aa aa'
    (VMut aa)    == (VMut aa')      = dynEq aa aa'
    (VExt aa)    == (VExt aa')      = dynEq aa aa'
    _            == _               = False

instance Ord Val where
    compare (VUndef aa) (VUndef aa') = compare aa aa'
    compare (VUndef aa) (VNative aa') = LT
    compare (VUndef aa) (VPure aa') = LT
    compare (VUndef aa) (VMut aa') = LT
    compare (VUndef aa) (VExt aa') = LT
    compare (VNative aa) (VUndef aa') = GT
    compare (VNative aa) (VNative aa') = compare aa aa'
    compare (VNative aa) (VPure aa') = LT
    compare (VNative aa) (VMut aa') = LT
    compare (VNative aa) (VExt aa') = LT
    compare (VPure aa) (VUndef aa') = GT
    compare (VPure aa) (VNative aa') = GT
    compare (VPure aa) (VPure aa') = dynCompare aa aa'
    compare (VPure aa) (VMut aa') = LT
    compare (VPure aa) (VExt aa') = LT
    compare (VMut aa) (VUndef aa') = GT
    compare (VMut aa) (VNative aa') = GT
    compare (VMut aa) (VPure aa') = GT
    compare (VMut aa) (VMut aa') = dynCompare aa aa'
    compare (VMut aa) (VExt aa') = LT
    compare (VExt aa) (VUndef aa') = GT
    compare (VExt aa) (VNative aa') = GT
    compare (VExt aa) (VPure aa') = GT
    compare (VExt aa) (VMut aa') = GT
    compare (VExt aa) (VExt aa') = dynCompare aa aa'

instance Data Val where
    toConstr (VUndef x)     = toConstr x
    toConstr (VNative x)    = toConstr x
    toConstr (VPure x)      = toConstr x
    toConstr (VMut x)       = toConstr x
    toConstr (VExt x)       = toConstr x
    dataTypeOf (VUndef x)   = dataTypeOf x
    dataTypeOf (VNative x)  = dataTypeOf x
    dataTypeOf (VPure x)    = dataTypeOf x
    dataTypeOf (VMut x)     = dataTypeOf x
    dataTypeOf (VExt x)     = dataTypeOf x
    gunfold                 = gunfold
