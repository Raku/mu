{-# OPTIONS_GHC -fglasgow-exts #-}{-! global : YAML_Pos, Perl6Class, MooseClass !-}
{-|
    Perl 6 Values.

>   There beryl, pearl, and opal pale,
>   And metal wrought like fishes' mail,
>   Buckler and corslet, axe and sword,
>   And shining spears were laid in hoard...
-}
module Pugs.Val (
    Val(..), ValUndef, ValNative, ValPure, ValMut, ValIO, Id
) where
import Pugs.Internals

{-|

This module contains the definition of the five variants for a Perl 6 value.
However, the actual constructors for each variant are abstract, and this module
does not provide concrete data type definitions beyond those five.

-}

-- | 'Val' represents what an unconstrained scalar container can hold.
data Val
    = VUndef  !ValUndef   -- ^ Values that defeat type constraints (ValId = 0)
    | VNative !ValNative  -- ^ Values that can fit into an UArray  (ValId = boxed value)
    | VPure   !ValPure    -- ^ Values that are immutable           (ValId = itself)
    | VMut    !ValMut     -- ^ In-memory mutable structures        (ValId = memory addr)
    | VIO     !ValIO      -- ^ Input/Ouput handles                 (ValId = impl. dep.)
    deriving (Show, Eq, Ord, Data, Typeable)

-- | 'Id' is an unique ID that distinguishes two @Val@s of the same type from each other.
type Id = ValPure

--------------------------------------------------------------------------------------

-- | L<S06/"Undefined types">
data ValUndef
    = UUndef                        -- ^ "my $x"
    | UWhatever                     -- ^ "my $x = *"
    | UFailure { f_errid :: !Id }   -- ^ "my $x = fail 'oops'"
    deriving (Show, Eq, Ord, Data, Typeable)

--------------------------------------------------------------------------------------

-- | Unboxed or native values. They have themselves as their .valid.
data ValNative
    = NBit      !NativeBit      -- ^ 0
    | NInt      !NativeInt      -- ^ -3
    | NUint     !NativeUint     -- ^ 7
    | NBuf      !NativeBuf      -- ^ (a raw chunk of ints or uints)
    | NNum      !NativeNum      -- ^ 4.2
    | NComplex  !NativeComplex  -- ^ (45 - 9i)
    deriving (Show, Eq, Ord, Data, Typeable)

type NativeBit      = Bool
type NativeBool     = Bool
type NativeInt      = Int
type NativeUint     = Word
type NativeBuf      = ByteString
type NativeNum      = Float
type NativeComplex  = () -- Complex NativeNum 

--------------------------------------------------------------------------------------
-- | L<S06/"Immutable types">
data ValPure
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

type MutScalar      = ()
type MutArray       = ()
type MutHash        = ()
type MutBuf         = ()
type MutRoutine     = ()
type MutVRule       = ()
type MutVMatch      = ()
type MutPackage     = ()
type MutModule      = ()
type MutClass       = ()
type MutRole        = ()
type MutGrammar     = ()
type MutObject      = ()
type MutDynamic     = ()

--------------------------------------------------------------------------------------
-- | Obviously side-effectual types such as file handles.
--   Computations on these types must take place in the IO monad.
data ValIO
    = IFile     !IOFile     -- ^ File handle
    | ISocket   !IOSocket   -- ^ Socket handle
    | IThread   !IOThread   -- ^ Thread handle
    | IProcess  !IOProcess  -- ^ Process handle
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type IOFile         = ()
type IOSocket       = ()
type IOThread       = ()
type IOProcess      = ()
