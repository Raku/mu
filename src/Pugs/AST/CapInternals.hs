{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances -fallow-incoherent-instances #-}

-- This is WIP towards an overhaul of Pugs.AST.Internals.
-- It includes a new and more detailed AST which captures much more Perl 6
-- semantics; captures; MOP based on Stevan++'s Moose.pm; and cleans up
-- some accrued cruft.

module Pugs.AST.CapInternals
where
import Pugs.Internals
import Pugs.Types hiding (Var) -- XXX throw that hiding out
import Pugs.Cont hiding (shiftT, resetT)
import System.IO.Error (try)
import Data.Typeable
import Data.Generics.Basics (Data(..), mkDataType)
import Data.Array.IO
import qualified Data.Set            as Set
import qualified Data.Map            as Map
import qualified Data.Seq            as Seq
import qualified Data.IntMap         as IntMap
import qualified Data.ByteString     as Buf
import qualified Data.IntSet         as IntSet
import qualified Data.Generics.Twins as Twins
import qualified Data.Version
import qualified Network.URI

import Pugs.Parser.Number
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5

import DrIFT.Perl6Class

--import {-# SOURCE #-} Pugs.AST.CapInternals.Instances

--type Str = Str.ByteString
type IntSet = IntSet.IntSet
type SeqOf = Seq.Seq

{- <DrIFT> Imports for the DrIFT
import Pugs.AST.Scope
import Pugs.AST.Pos
import Pugs.AST.Prag
import Pugs.AST.SIO
import Pugs.Types hiding (Var)
import Pugs.Internals
import Pugs.Embed.Perl5
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import Data.Array.IO
import DrIFT.Perl6Class
import qualified Data.Version
import qualified Network.URI

-- instance (Typeable a, Show a) => Perl6Class (IOThread a)
instance (PLit a) => PLit (Eval a) where -- XXX: very bogus
    plShow (Eval x) = plShow x

instance (PLit a) => PLit (IOThread a) where -- XXX
    plShow (IOThread x) = "<thread: " ++ (show $ plShow x) ++ ">"
 </DrIFT> -}
 
data Eval a = Eval a -- junk; just for testing p6 derivations
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
-- instance (Perl6Class a) => Perl6Class (Eval a)

{-
-}


type VList = [Val]
type VSubst = (VRule, Exp)
type VArray = [Val]
type VHash = Map VStr Val
data VThunk = MkThunk
    { thunkExp   :: Eval Val
    , thunkClass :: Class
    }
    deriving (Typeable) {-!derive: YAML_Pos!-}
newtype VProcess = MkProcess (ProcessHandle)
    deriving (Typeable) {-!derive: YAML_Pos!-}

{-|
Representation for rules (i.e. regexes).

Currently there are two types of rules: Perl 5 rules, implemented with PCRE,
and Perl 6 rules, implemented with PGE.
-}
data VRule
    -- | Perl5-compatible regular expression
    = MkRulePCRE
        { rxRegex     :: !Regex -- ^ The \'regular\' expression (as a PCRE
                                --     'Regex' object)
        , rxGlobal    :: !Bool  -- ^ Flag indicating \'global\' (match-all)
        , rxNumSubs   :: !Int   -- ^ The number of subpatterns present.
        , rxStringify :: !Bool
        , rxRuleStr   :: !String -- ^ The rule string, for user reference.
        , rxAdverbs   :: !Val
        }
    -- | Parrot Grammar Engine rule
    | MkRulePGE
        { rxRule      :: !String -- ^ The rule string
        , rxGlobal    :: !Bool   -- ^ Flag indicating \'global\' (match-all)
        , rxStringify :: !Bool
        , rxAdverbs   :: !Val
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Represents a value.
data Val
    = VNative ValNative          -- ^ Native, unboxed values
    | VUndef  ValUndef           -- ^ Various undefined values
    | VPure   ValPure            -- ^ Immutable (or "pure") values
    | VMut    ValMut             -- ^ Mutable variables (in STM monad)
    | VIO     ValIO              -- ^ I/O handles (in IO monad)
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type ExpVal = Val

-- | Unboxed or native values. They have themselves as their .valid.
type ValNative = Native
data Native
    = NBit  !NativeBit     -- ^ 0
    | NInt  !NativeInt     -- ^ -3
    | NUint !NativeUint    -- ^ 7
    | NBuf  !NativeBuf     -- ^ (a raw chunk of ints or uints)
    | NNum  !NativeNum     -- ^ 4.2
    | NCplx !NativeComplex -- ^ (45 - 9i)
    | NBool !NativeBool    -- ^ True (same underlying storage as NBit + True/False)
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | L<S06/"Undefined types">
data ValUndef
    = UUndef               -- ^ e.g., "my $x" with out further assignment
    | UWhatever            -- ^ e.g. the * in 1 .. *
    | UFailure    !ObjId   -- ^ $! object
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


type NativeBit   = Bool
type NativeBool  = Bool
type NativeInt   = Int
type NativeUint  = Word
type NativeUInt  = Word

data Sign
    = SPositive
    | SNegative
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data PureInt
    = IFinite      !Integer
    | IInfinite    !Sign
    | INotANumber
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data PureNum
    = NRational  !Rational
    | NFloat     !Float
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type NativeNum = Float
type NativeComplex = ComplexNum NativeNum
type PureComplex   = ComplexNum PureNum

-- Inf or NaN if either part is Inf or NaN.
data ComplexNum a = MkComplexNum
    { c_real      :: !a
    , c_imaginary :: !a
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

newtype PureList = MkList { l_list :: SeqOf ListComponent }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data ListComponent
    = CSeq   PureSeq
    | CRange PureRange
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

newtype PureSeq = MkSeq { s_seq :: SeqOf Val }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data PureRange = MkRange
    { r_from :: Val  -- ??
    , r_to   :: Val  -- ??
    , r_next :: Code -- ??
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- Buffer in memory (Perl type Buf).
-- This is called MemBuf because of a clash with Pugs.Types.Buf .
data MemBuf = MkBuf { b_buffer :: IOUArray Word64 Word8 }
    deriving (Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass !-}
{- <DrIFT>
instance PLit MemBuf where
    plShow _       = "<buf>"
instance Show (IOUArray Word64 Word8) where
    show _ = "<buf>"
</DrIFT> -}
instance Show MemBuf where
    show _         = "<buf>"
instance Ord MemBuf where
    compare _      = error "can't compare MemBuf"
instance Eq MemBuf where
    (==) _ _       = error "can't equate MemBuf"
instance Data MemBuf where
    gfoldl _ _ _   = error "can't gfoldl MemBuf"
    gunfold _ _ _  = error "can't gfoldl MemBuf"
    toConstr _     = error "can't gfoldl MemBuf"
    dataTypeOf _   = mkDataType "Pugs.AST.CapInternals.MemBuf" [] -- bogus

type NativeBuf     = Buf.ByteString
type PureBuf       = MemBuf

type PureBit       = Bool
type PureStr       = Str
type PureBool      = Bool
type PureException = String -- XXX *very* bogus
type PureCode      = String -- XXX *very* bogus
type PureSig       = Sig
type PureCap       = Cap

newtype PureSet = MkSet { s_set :: Set Val }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

{-|
Represents a junction value.

Note that @VJunc@ is also a pun for a 'Val' constructor /containing/ a 'VJunc'.
-}
data PureJunc = MkJunc
    { j_type :: !JuncType -- ^ 'JAny', 'JAll', 'JNone' or 'JOne'
    , j_dup  :: !PureSet
    -- ^ Only used for @one()@ junctions. Contains those values
    --     that appear more than once (the actual count is
    --     irrelevant), since matching any of these would
    --     automatically violate the 'match /only/ one value'
    --     junctive semantics.
    , j_set  :: !PureSet
    -- ^ Set of values that make up the junction. In @one()@
    --     junctions, contains the set of values that appear exactly
    --     /once/.
    }
    deriving (Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | The combining semantics of a junction. See 'VJunc' for more info.
data JuncType = JAny  -- ^ Matches if /at least one/ member matches
              | JAll  -- ^ Matches only if /all/ members match
              | JNone -- ^ Matches only if /no/ members match
              | JOne  -- ^ Matches if /exactly one/ member matches
    deriving (Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data PurePair = MkPair -- ?? or is this more efficient? data Pair (Val, Val)
    { p_key :: Val
    , p_val :: Val
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- XXX what about ordered mappings?
data PureMap = Map Val Val
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

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

type MutScalar  = Eval ValPure
type MutArray   = Eval [ValPure]
type MutHash    = Eval (Map ValPure Val)
type MutBuf     = Eval MemBuf
type MutRoutine = Eval Routine
type MutVRule   = Eval VRule
type MutVMatch  = Eval Bogus
type MutPackage = Eval Bogus
type MutModule  = Eval Bogus
type MutClass   = Eval Bogus
type MutRole    = Eval Bogus
type MutGrammar = Eval Bogus
type MutDynamic = Eval Bogus

data Bogus = Bogus
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

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

data IOFile = Handle
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
data IOSocket = Socket
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
data IOThread a = IOThread a -- XXX
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
data IOProcess = ProcessHandle
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Obviously side-effectual types such as file handles.
--   Computations on these types must take place in the IO monad.
data ValIO
    = IFile     !IOFile           -- ^ File handle
    | ISocket   !IOSocket         -- ^ Socket handle
    | IThread   !(IOThread Val)   -- ^ Thread handle
    | IProcess  !IOProcess        -- ^ PID value
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

{-
instance ScalarClass a where
    doScalarFetch :: a -> Eval Val
    doScalarStore :: a -> Eval Val

_scalar_fetch :: Val -> Eval Val
_scalar_fetch v@Native{} = return v
_scalar_fetch v@Pure{}   = return v
_scalar_fetch v@(Mut m)  = case m of
    VMScalar s  -> doScalarFetch s
    VMArray{}   -> return v
    VMObject o  -> callMethod "scalar_fetch" o
    _           -> fail ""
_scalar_fetch v@(IO i)   = fail "not implemented: scalar_fetch on IO value"


array_flatten :: Val -> Eval Val
array_flatten v@Native{} = return v
array_flatten v@Pure{} = return v
array_flatten v@(Mut m) = case m of
    VMScalar s  -> array_flatten `withScalar` s
    VMArray  a  -> doArrayFlatten a
    VMObject o  -> callMethod "array_flatten" o
    _           -> fail ""
array_flatten v@(IO i)   = fail "not implemented: array_flatten on IO value"
array_subscript :: Val -> Val -> Eval Val
array_subscript v@Native{} = return v
array_subscript idx v@Pure{} = return v
array_subscript idx v@(Mut m) = case m of
    VMScalar s  -> (array_subscript idx) `withScalar` s
    VMArray  a  -> doArraySubscript idx a
    VMObject o  -> callMethod "array_subscript" o [idx]
    _           -> fail ""
array_subscript v@(IO i)   = fail "not implemented: array_subscript on IO value"


hash_flatten :: Val -> Eval Val
hash_flatten v@Native{} = return v
hash_flatten v@Pure{} = return v
hash_flatten v@(Mut m) = case m of
    VMScalar s  -> hash_flatten `withScalar` s
    VMHash   a  -> doHashFlatten a
    VMObject o  -> callMethod "hash_flatten" o
    _           -> fail ""
hash_flatten v@(IO i)   = fail "not implemented: hash_flatten on IO value"
hash_subscript :: Val -> Val -> Eval Val
hash_subscript v@Native{} = return v
hash_subscript idx v@Pure{} = return v
hash_subscript idx v@(Mut m) = case m of
    VMScalar s -> (hash_subscript idx) `withScalar` s
    VMHash   a -> doHashSubscript idx a
    VMObject o -> callMethod "hash_subscript" o [idx]
    _          -> fail ""
hash_subscript v@(IO i)   = fail "not implemented: hash_subscript on IO value"


instance ArrayClass (TVar Val) where
    doArrayFlatten = error "XXX"


instance ScalarClass (TVar Val) where
    -- vivify?
    doScalarFetch t = do
        lv <- asks envLValue
        case lv of
            RValue -> liftSTM (readTVar t)
            LValue typ -> do
                rv <- readTVar t
                case rv of
                    Pure VPUndef -> do
                        writeTVar t =<< fmap Mut (newLValue typ)
                    _ -> return rv
-}

instance Show JuncType where
    show JAny  = "any"
    show JAll  = "all"
    show JNone = "none"
    show JOne  = "one"

instance Show PureJunc where
    show (MkJunc jtype _ (MkSet set)) =
        (show jtype) ++ "(" ++
            (foldl (\x y ->
                if x == "" then show y
                else x ++ "," ++ show y)
            "" $ Set.elems set) ++ ")"

{-|
Each 'VCode' structure has a 'SubType' indicating what \'level\' of
callable item it is. 'doApply' uses this to figure out how to enter
the proper scope and 'Env' when the sub is called.

Note that this is the \'type\' of a \'sub\', and has nothing to do with
subtyping.
-}
data SubType = SubMethod    -- ^ Method
             | SubCoroutine -- ^ Coroutine
             | SubMacro     -- ^ Macro
             | SubRoutine   -- ^ Regular subroutine
             | SubBlock     -- ^ Bare block
             | SubPointy    -- ^ Pointy sub
             | SubPrim      -- ^ Built-in primitive operator (see "Pugs.Prim")
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos !-}

-- | A list of formal parameters.
type Params     = [Param]
{-|
A list of bindings from formal parameters ('Param') to actual parameter
expressions ('Exp').
-}
type Bindings   = [(Param, Exp)]
{-|
A sub that has a non-empty 'SlurpLimit' is a bound (or partially bound) sub
that has a finite number of slurpy scalar params bound, and no slurpy array
param bound (see 'VCode' and "Pugs.Bind").

Each list entry consists of the number of slurpable args expected, and an
expression that will evaluate to the actual list of slurpable args.
When the sub is called (see 'Pugs.Eval.apply'), the expression is evaluated.
If it evaluates to /too many/ args, the call will fail.

This needs to be a list (rather than a @Maybe@) because Perl 6's @.assuming@
(i.e. explicit currying) means that a sub can have its arguments bound in
separate stages, and each of the bindings needs to be checked.

>[12:02] <autrijus> scook0: .assuming will impose multiple limits
>[12:02] <autrijus> because you can assume (curry) multiple times
>[12:02] <scook0> ah
>[12:02] <scook0> I'll have to write that in the docs then
>[12:03] <scook0> Am I correct in that they only apply to subs that take a finite number of slurpy scalars?
>[12:04] <scook0> Slurpy array params seem to nuke the SlurpLimit
>[12:04] <scook0> because slurpy arrays can take any number of args
>[12:07] <autrijus> scook0: yes, and yes.
-}
type SlurpLimit = [(VInt, Exp)]

instance Ord VComplex where
    compare (a :+ ai) (b :+ bi) = compare (a, ai) (b, bi)

instance (Typeable a) => Show (TVar a) where
    show _ = "<ref>"

type Ident = Str -- XXX wrong

-- | General purpose mapping from identifiers to values.
type Table = Map Ident Val

-- | AST for a statement. The top level of an AST is a list of Stmt.
data Stmt = MkStmt
    { label      :: Maybe Ident
    , pragmas    :: Table
    , expression :: Exp
    } deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Carry over last pragmas and create a new statement out of an expression
nextStmt :: Stmt -> Exp -> Stmt
nextStmt MkStmt{ pragmas=prag } exp = MkStmt{ label=Nothing, pragmas=prag, expression=exp }

-- | AST for an expression.
data Exp
    = ENoop                            -- ^ No-op
    | EVar      ExpVar                 -- ^ Variable
    | EVal      ExpVal                 -- ^ Value
    | EDeref    ExpVar                 -- ^ Dereference
    | EBind     Exp  Exp               -- ^ Bind, i.e., :=
    | EAssign   Exp  Exp               -- ^ Assignment, =
    | EControl  ExpControl             -- ^ Control structure, e.g. if, while
    | EFlatten  [Exp]                  -- ^ Wrapper for expressions forced into
                                       --   slurpy context
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


-- | Control statement, such as "if".
data ExpControl
    = CCall        Ident  Cap           -- ^ lookup a routine, call it
    | CApply       Exp  Cap             -- ^ apply a Code without lookup
    | CCond        Exp  Code            -- ^ 2 if 1
    | CTrenaryCond Exp  Code  Code      -- ^ 1 ?? 2 !! 3
    | CCondBlock   (Exp, Code) [(Exp, Code)] (Maybe Code)
                                        -- ^ if 1 { 2 } else { 3 } or in general,
                                        --   if 1 { 2 } elsif 3 { 4 } elsif 5 { 6 } 7
                                        -- ^ &statement_control:<if>
    | CGoto        Ident                -- ^ &statement_control:<goto>
    | CWhile       Exp  Code            -- ^ &statement_control:<while>
    | CGiven       Exp  Code            -- ^ given
    | CWhen        Exp  Code            -- ^ when
    | CForeign                          -- ^ &statement_control:<mycontrol>
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Single parameter for a function/method, e.g.:
--   Elk $m where { $m.antlers ~~ Velvet }
{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual argument
values.
-}
data Param = MkParam
    { p_variable    :: Ident         -- ^ E.g. $m above
    , p_types       :: [Class]       -- ^ Static pieces of inferencer-food
                                     --   E.g. Elk above
    , p_constraints :: [Code]        -- ^ Dynamic pieces of runtime-mood
                                     --   E.g. where {...} above
    , p_unpacking   :: Maybe Sig     -- ^ E.g. BinTree $t (Left $l, Right $r)
    , p_default     :: Maybe Exp     -- ^ E.g. $answer? = 42
    , p_label       :: Ident         -- ^ E.g. :mode
    , p_slots       :: Table         -- ^ Any additional attrib not
                                     --   explicitly mentioned below
    , p_hasAccess   :: ParamAccess   -- ^ is ro, is rw, is copy
    , p_isRef       :: Bool
    , p_isLazy      :: Bool
    } deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data ParamAccess
    = AccRO
    | AccRW
    | AccCopy
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Function associtivity
data CodeAssoc
    = AssLeft
    | AssRight
    | AssNon
    | AssChain
    | AssList
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | AST for function signature. Separated to method and function variants
--   for ease of pattern matching.
data Sig
    = SigMethSingle
        { s_invocant                  :: Param
        , s_requiredPositionalCount   :: Int
        , s_requiredNames             :: Set Ident
        , s_positionalList            :: [Param]
        , s_namedSet                  :: Map Ident Param
        , s_slurpyScalarList          :: [Param]
        , s_slurpyArray               :: Maybe Param
        , s_slurpyHash                :: Maybe Param
        , s_slurpyCode                :: Maybe Param
        , s_slurpyCapture             :: Maybe Param
        }
    | SigSubSingle
        { s_requiredPositionalCount   :: Int
        , s_requiredNames             :: Set Ident
        , s_positionalList            :: [Param]
        , s_namedSet                  :: Map Ident Param
        , s_slurpyScalarList          :: [Param]
        , s_slurpyArray               :: Maybe Param
        , s_slurpyHash                :: Maybe Param
        , s_slurpyCode                :: Maybe Param
        , s_slurpyCapture             :: Maybe Param
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


newtype CodeWrapping = MkWrapping
    { w_wrappings :: TVar (IntMap Routine)
    }
    deriving (Show, Eq, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

instance Ord CodeWrapping where
    compare _ _ = error "can't compare CodeWrapping"

data Routine
    = RoutineSimple
        { wrappings       :: CodeWrapping
        , routineCode     :: Code
        }
    | RoutineMulti
        { wrappings       :: CodeWrapping
        , routineVariants :: Set MultiVariant
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

{- (It's not clear how multi variants are supposed to be ordered.
   One thing is that we want local code to be able to add variants,
   but we also presumably want to express guards with multimethods.
-}

-- | AST for a primitive Code object
data Code
    = CodePerl
        { c_signature         :: Sig
        , c_precedence        :: Rational
        , c_assoc             :: CodeAssoc
        , c_isRW              :: Bool
        , c_isSafe            :: Bool
        , c_isCached          :: Bool
        , c_body              :: [Stmt]    -- ^ AST of "do" block
        , c_pad               :: Pad       -- ^ Storage for lexical vars
        , c_traits            :: Table     -- ^ Any additional trait not
                                           --   explicitly mentioned below
        , c_preBlocks         :: [Code]
        , c_postBlocks        :: [Code]
        , c_firstBlocks       :: [Code]
        , c_lastBlocks        :: [Code]
        , c_nextBlocks        :: [Code]
        , c_keepBlocks        :: [Code]
        , c_undoBlocks        :: [Code]
        }
    | CodePrim
        { c_signature         :: Sig
        , c_precedence        :: Rational
        , c_assoc             :: CodeAssoc
        , c_isRW              :: Bool
        , c_isSafe            :: Bool
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}
    
data MultiVariant = MkMultiVariant 
    { m_semicolonOffsets              :: IntSet
    , m_callable                      :: Code  -- ^ Thing actually called
    , m_extraWrappings                :: Maybe CodeWrapping
    } deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


-- | Storage cell for a lexical variable: @Pad@, @EntryDeclarator@, @EntryStorage@, @PadEntry@

{- Pad -}
{-|
A 'Pad' keeps track of the names of all currently-bound symbols, and
associates them with the things they actually represent.

It is represented as a mapping from names to /lists/ of bound items.
This is to allow for multi subs, because we will need to keep
/multiple/ subs associated with one symbol. In other cases, the list
should just contain a single value. See 'Pugs.AST.genSym' and 'Pugs.AST.genMultiSym' for
more details.

@TVar@ indicates that the mapped-to items are STM transactional variables.

Pads are stored in the current 'Code', and lexical lookups proceed through
progressively outer scopes until an item is found. For dynamic variables
(e.g., "our"), the pad holding the items is located in the package.
-}

newtype Pad = MkPad { padEntries :: Map Var PadEntry }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type EntryStorage = TVar Val
instance Ord EntryStorage where
    compare _ = error "can't compare EntryStorage"


data EntryDeclarator
    = DeclMy
    | DeclOur
    | DeclHas
    | DeclState
    | DeclConstant
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data PadEntry = MkEntry
    { e_declarator :: EntryDeclarator   -- ^ my etc.
    , e_storage    :: EntryStorage      -- ^ stored value
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type Bit = Bool     -- XXX: correct?

type ObjId      = Native
type ObjSlots   = TVar Table
type ObjClass   = Class
type ObjPayload = MutDynamic

instance Ord ObjSlots where
    compare _ = error "can't compare ObjSlots"

data MutObject
    = ObjInstance
        { o_id       :: !ObjId      -- ^ our unique id
        , o_meta     :: !ObjClass   -- ^ id of our metaobj/type
        , o_slots    :: !ObjSlots   -- ^ storage for explicit fields
        }
    | MkForeign
        { o_id       :: !ObjId   -- ^ our unique id
        , o_meta     :: !ObjClass   -- ^ id of our metaobj/type
        , o_payload  :: !ObjPayload -- ^ storage for opaque wrapped obj
        }
    | MkPrototype
        { o_id       :: !ObjId      -- ^ our unique id
        , o_meta     :: !ObjClass   -- ^ id of our metaobj/type
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Capture.
data Cap 
    = CaptMeth
        { c_invocant :: Exp
        , c_argstack :: [Arglist]
        }
    | CaptSub
        { c_argstack :: [Arglist]
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data Arglist = MkArglist
    { a_positional :: [Exp]
    , a_named      :: Map Str [Exp]
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data Var
    = VarLexical
        { v_name        :: Ident
        , v_callerCount :: Int
        , v_outerCount  :: Int
        }
    | VarDynamic
        { v_name        :: Ident
        , v_packageName :: [Ident]
        }
    | VarMagic
        { v_magic       :: Magic
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type ExpVar = Var

data Magic
    = MOS               -- ^ $?OS        Which os am I compiled for?
    | MOSVer            -- ^ $?OSVER     Which os version am I compiled for?
    | MPerlVer          -- ^ $?PERLVER   Which Perl version am I compiled for?
    | MFile             -- ^ $?FILE      Which file am I in?
    | MLine             -- ^ $?LINE      Which line am I at?
    | MScalarPackage    -- ^ $?PACKAGE   Which package am I in?
    | MArrayPackages    -- ^ @?PACKAGE   Which packages am I in?
    | MScalarModule     -- ^ $?MODULE    Which module am I in?
    | MArrayModules     -- ^ @?MODULE    Which modules am I in?
    | MScalarClass      -- ^ $?CLASS     Which class am I in? (as variable)
    | MArrayClasses     -- ^ @?CLASS     Which classes am I in?
    | MScalarRole       -- ^ $?ROLE      Which role am I in? (as variable)
    | MArrayRoles       -- ^ @?ROLE      Which roles am I in?
    | MScalarGrammar    -- ^ $?GRAMMAR   Which grammar am I in?
    | MArrayGrammars    -- ^ @?GRAMMAR   Which grammars am I in?
    | MParser           -- ^ $?PARSER    Which Perl grammar was used to
                        -- ^                   parse this statement?
    | MScalarRoutine    -- ^ &?ROUTINE   Which routine am I in?
    | MArrayRoutines    -- ^ @?ROUTINE   Which routines am I in?
    | MScalarBlock      -- ^ &?BLOCK     Which block am I in?
    | MArrayBlocks      -- ^ @?BLOCK     Which blocks am I in?
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data Package = MkPackage
    { p_name     :: Ident
    , p_parent   :: Maybe Package
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data Module = MkModule
    { m_version   :: Data.Version.Version
    , m_authority :: Maybe Network.URI.URI
    , m_package   :: Maybe Package
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data Class = MkClass
    { c_module              :: Maybe Module
    , c_superClasses        :: [Class]
    , c_runtimeSuperClasses :: Eval [Class] -- list of runtime-added superclasses
    , c_methodTable         :: Map Ident Code
    , c_runtimeMethodtable  :: Eval (Map Ident Code)
    , c_runtimeSlots        :: Eval (Map Ident (TVar Val))
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

newtype Grammar = MkGrammar { g_class :: Class }

newtype Role    = MkRole    { r_role  :: Class }

instance Ord  Network.URI.URI
instance Data Data.Version.Version
-- where compare (URI sc au pa qu fr) (URI sc' au' pa' qu' fr') =

instance Typeable Unique where typeOf _ = typeOf ()
instance Typeable ProcessHandle where typeOf _ = typeOf ()
#if __GLASGOW_HASKELL__ <= 604
instance Typeable1 Tree where typeOf1 _ = typeOf ()
#endif

{- <DrIFT>
instance YAML (VThread Val)
instance YAML ClassTree
instance YAML Dynamic
instance YAML Pragma
instance YAML Regex
instance YAML VComplex
instance YAML PerlSV
instance YAML Float
instance YAML Table
instance YAML IntSet
instance YAML (Map ValPure Val)
instance YAML (Map Ident Param)
instance YAML (Set Ident)
instance YAML (Set Val)
instance YAML (SeqOf Val)
instance YAML (SeqOf ListComponent)
instance YAML (Set MultiVariant)
instance YAML (Map Var PadEntry)
instance YAML (Map Str [Exp])
instance YAML (IOUArray Word64 Word8)
instance YAML (TVar (IntMap Routine))
instance YAML ProcessHandle
instance YAML Data.Version.Version
instance YAML Network.URI.URI
instance YAML (Map Ident (TVar Val))
instance YAML (Map Ident Code)

</DrIFT> -}
