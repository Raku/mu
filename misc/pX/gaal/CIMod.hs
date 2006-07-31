{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}

-- This is WIP towards an overhaul of Pugs.AST.Internals.
-- It includes a new and more detailed AST which captures much more Perl 6
-- semantics; captures; MOP based on Stevan++'s Moose.pm; and cleans up
-- some accrued cruft.

module CIMod
where
import Pugs.Internals
import Pugs.Types hiding (Var) -- XXX throw that hiding out
import Pugs.Cont hiding (shiftT, resetT)
import System.IO.Error (try)
import Data.Typeable
import Data.Generics.Basics (Data(..), mkDataType)
import Data.Array.IO
import Data.Yaml.Syck
import DrIFT.YAML
import qualified Data.Set            as Set
import qualified Data.Map            as Map
import qualified Data.Seq            as Seq
import qualified Data.IntMap         as IntMap
import qualified Data.ByteString     as Str
import qualified Data.IntSet         as IntSet
import qualified Data.Generics.Twins as Twins

import Pugs.Parser.Number
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5

import DrIFT.Perl6Class


--import {-# SOURCE #-} Pugs.AST.CapInternals.Instances

type Str = Str.ByteString
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
 </DrIFT> -}
 
data (Perl6Class a) => Eval a = Eval a -- junk; just for testing p6 derivations
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos !-}
type Class = Bogus -- junk; just for testing p6 derivations
instance (Perl6Class a) => Perl6Class (Eval a) where
    asPerl6Object (Eval x) = asPerl6Object x

instance (Perl6Class a, PLit a) => PLit (Eval a) where -- XXX: very bogus
    plShow (Eval x) = plShow x

instance (PLit a) => PLit (IOThread a) where -- XXX
    plShow (IOThread x) = "<thread: " ++ (show $ plShow x) ++ ">"


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

-- | Unboxed or native values. They have themselves as their .id.
type ValNative = Native
data Native
    = NBit  !NativeBit     -- ^ 0
    | NInt  !NativeInt     -- ^ -3
    | NUint !NativeInt     -- ^ 7
    | NBuf  !NativeBuf     -- ^ (a raw chunk of ints or uints)
    | NNum  !NativeNum     -- ^ 4.2
    | NCplx !NativeComplex -- ^ (45 - 9i)
    | NStr  !NativeStr     -- ^ 'aloha'
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

data Sign
    = SPositive
    | SNegative
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data NativeInt
    = IFinite      !Integer
    | IInfinite    !Sign
    | INotANumber
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data NativeNum
    = NRational  !Rational
    | NFloat     !Float
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type NativeStr = Str

-- Inf or NaN if either part is Inf or NaN.
data NativeComplex = MkComplex
    { c_real      :: !NativeNum
    , c_imaginary :: !NativeNum
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data PureList = MkList
    { l_seq   :: !PureSeq
    , l_range :: !PureRange
    }
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
instance Show MemBuf where
    show _ = "<buf>"
instance Ord MemBuf where
    compare _ = error "can't compare MemBuf"
instance Eq MemBuf where
    (==) _ _ = error "can't equate MemBuf"
instance Data MemBuf where
    gfoldl _ _ _ = error "can't gfoldl MemBuf"
    gunfold _ _ _ = error "can't gfoldl MemBuf"
    toConstr _ = error "can't gfoldl MemBuf"
    dataTypeOf _ = mkDataType "Pugs.AST.CapInternals.MemBuf" [] -- bogus

type NativeBuf = MemBuf
type PureBuf   = MemBuf

type PureBit   = Bool
type PureInt   = Integer
type PureNum   = NativeNum -- XXX wrong?
type PureComplex  = NativeComplex -- XXX wrong?
type PureStr   = Str
type PureBool   = Bool
type PureException = String -- XXX *very* bogus
type PureCode = String -- XXX *very* bogus
type PureSig = Sig
type PureCap = Cap

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
instance PLit ValPure
instance PLit Val

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
instance YAML PureSet
instance YAML PureSeq
instance YAML (Set MultiVariant)
instance YAML (Map Var PadEntry)
instance YAML (Map Str [Exp])
instance YAML (IOUArray Word64 Word8)
instance YAML (TVar (IntMap Routine))
instance YAML CodeWrapping
instance YAML Pad
instance YAML ProcessHandle

</DrIFT> -}
{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}




{- 
-- WARNING WARNING WARNING --

This is an autogenerated file from src/Pugs/AST/CapInternals.hs.

Do not edit this file.

All changes made here will be lost!

-- WARNING WARNING WARNING --
-}

#ifndef HADDOCK








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
instance YAML PureSet
instance YAML PureSeq
instance YAML (Set MultiVariant)
instance YAML (Map Var PadEntry)
instance YAML (Map Str [Exp])
instance YAML (IOUArray Word64 Word8)
instance YAML (TVar (IntMap Routine))
instance YAML CodeWrapping
instance YAML Pad
instance YAML ProcessHandle

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance (Perl6Class a,YAML a) => YAML (Eval a) where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Eval" -> do
	    let ESeq [aa] = e
	    liftM Eval (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Eval"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Eval aa) = asYAMLseq "Eval" [asYAML aa]

instance (Perl6Class a,MooseClass a) => MooseClass (Eval a) where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Eval"
	    , showMooseClassDef ns "Eval" "Eval" [("","aa","Var \"a\"")]
	    ]

instance YAML VThunk where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkThunk" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkThunk (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkThunk"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkThunk aa ab) = asYAMLseq "MkThunk" [asYAML aa, asYAML ab]

instance YAML VProcess where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkProcess" -> do
	    let ESeq [aa] = e
	    liftM MkProcess (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkProcess"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkProcess aa) = asYAMLseq "MkProcess" [asYAML aa]

instance YAML VRule where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkRulePCRE" -> do
	    let liftM6 f m1 m2 m3 m4 m5 m6 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6)}
	    let ESeq [aa, ab, ac, ad, ae, af] = e
	    liftM6 MkRulePCRE (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af)
	"MkRulePGE" -> do
	    let ESeq [aa, ab, ac, ad] = e
	    liftM4 MkRulePGE (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkRulePCRE","MkRulePGE"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkRulePCRE aa ab ac ad ae af) = asYAMLseq "MkRulePCRE"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af]
    asYAML (MkRulePGE aa ab ac ad) = asYAMLseq "MkRulePGE"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad]

instance Perl6Class VRule where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "VRule"
	    , showPerl6ClassDef ns "VRule" "MkRulePCRE" [("Regex","$.rxRegex",""),("Bool","$.rxGlobal",""),("Int","$.rxNumSubs",""),("Bool","$.rxStringify",""),("String","$.rxRuleStr",""),("Val","$.rxAdverbs","")]
	    , showPerl6ClassDef ns "VRule" "MkRulePGE" [("String","$.rxRule",""),("Bool","$.rxGlobal",""),("Bool","$.rxStringify",""),("Val","$.rxAdverbs","")]
	    ]
    asPerl6Object MkRulePCRE {} = error $ "not yet: " ++ "Body {constructor = \"MkRulePCRE\", labels = [\"rxRegex\",\"rxGlobal\",\"rxNumSubs\",\"rxStringify\",\"rxRuleStr\",\"rxAdverbs\"], types = [Con \"Regex\",Con \"Bool\",Con \"Int\",Con \"Bool\",Con \"String\",Con \"Val\"]}"
    asPerl6Object MkRulePGE {} = error $ "not yet: " ++ "Body {constructor = \"MkRulePGE\", labels = [\"rxRule\",\"rxGlobal\",\"rxStringify\",\"rxAdverbs\"], types = [Con \"String\",Con \"Bool\",Con \"Bool\",Con \"Val\"]}"

instance MooseClass VRule where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "VRule"
	    , showMooseClassDef ns "VRule" "MkRulePCRE" [("Regex","rxRegex",""),("Bool","rxGlobal",""),("Int","rxNumSubs",""),("Bool","rxStringify",""),("String","rxRuleStr",""),("Val","rxAdverbs","")]
	    , showMooseClassDef ns "VRule" "MkRulePGE" [("String","rxRule",""),("Bool","rxGlobal",""),("Bool","rxStringify",""),("Val","rxAdverbs","")]
	    ]

instance YAML Val where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"VNative" -> do
	    let ESeq [aa] = e
	    liftM VNative (fromYAML aa)
	"VUndef" -> do
	    let ESeq [aa] = e
	    liftM VUndef (fromYAML aa)
	"VPure" -> do
	    let ESeq [aa] = e
	    liftM VPure (fromYAML aa)
	"VMut" -> do
	    let ESeq [aa] = e
	    liftM VMut (fromYAML aa)
	"VIO" -> do
	    let ESeq [aa] = e
	    liftM VIO (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["VNative","VUndef","VPure","VMut","VIO"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (VNative aa) = asYAMLseq "VNative" [asYAML aa]
    asYAML (VUndef aa) = asYAMLseq "VUndef" [asYAML aa]
    asYAML (VPure aa) = asYAMLseq "VPure" [asYAML aa]
    asYAML (VMut aa) = asYAMLseq "VMut" [asYAML aa]
    asYAML (VIO aa) = asYAMLseq "VIO" [asYAML aa]

instance Perl6Class Val where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Val"
	    , showPerl6ClassDef ns "Val" "VNative" [("ValNative","$.aa","")]
	    , showPerl6ClassDef ns "Val" "VUndef" [("ValUndef","$.aa","")]
	    , showPerl6ClassDef ns "Val" "VPure" [("ValPure","$.aa","")]
	    , showPerl6ClassDef ns "Val" "VMut" [("ValMut","$.aa","")]
	    , showPerl6ClassDef ns "Val" "VIO" [("ValIO","$.aa","")]
	    ]
    asPerl6Object (VNative aa) = "VNative.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (VUndef aa) = "VUndef.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (VPure aa) = "VPure.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (VMut aa) = "VMut.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (VIO aa) = "VIO.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass Val where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Val"
	    , showMooseClassDef ns "Val" "VNative" [("ValNative","aa","")]
	    , showMooseClassDef ns "Val" "VUndef" [("ValUndef","aa","")]
	    , showMooseClassDef ns "Val" "VPure" [("ValPure","aa","")]
	    , showMooseClassDef ns "Val" "VMut" [("ValMut","aa","")]
	    , showMooseClassDef ns "Val" "VIO" [("ValIO","aa","")]
	    ]

instance YAML Native where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"NBit" -> do
	    let ESeq [aa] = e
	    liftM NBit (fromYAML aa)
	"NInt" -> do
	    let ESeq [aa] = e
	    liftM NInt (fromYAML aa)
	"NUint" -> do
	    let ESeq [aa] = e
	    liftM NUint (fromYAML aa)
	"NBuf" -> do
	    let ESeq [aa] = e
	    liftM NBuf (fromYAML aa)
	"NNum" -> do
	    let ESeq [aa] = e
	    liftM NNum (fromYAML aa)
	"NCplx" -> do
	    let ESeq [aa] = e
	    liftM NCplx (fromYAML aa)
	"NStr" -> do
	    let ESeq [aa] = e
	    liftM NStr (fromYAML aa)
	"NBool" -> do
	    let ESeq [aa] = e
	    liftM NBool (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["NBit","NInt","NUint","NBuf","NNum","NCplx","NStr","NBool"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (NBit aa) = asYAMLseq "NBit" [asYAML aa]
    asYAML (NInt aa) = asYAMLseq "NInt" [asYAML aa]
    asYAML (NUint aa) = asYAMLseq "NUint" [asYAML aa]
    asYAML (NBuf aa) = asYAMLseq "NBuf" [asYAML aa]
    asYAML (NNum aa) = asYAMLseq "NNum" [asYAML aa]
    asYAML (NCplx aa) = asYAMLseq "NCplx" [asYAML aa]
    asYAML (NStr aa) = asYAMLseq "NStr" [asYAML aa]
    asYAML (NBool aa) = asYAMLseq "NBool" [asYAML aa]

instance Perl6Class Native where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Native"
	    , showPerl6ClassDef ns "Native" "NBit" [("NativeBit","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NInt" [("NativeInt","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NUint" [("NativeInt","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NBuf" [("NativeBuf","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NNum" [("NativeNum","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NCplx" [("NativeComplex","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NStr" [("NativeStr","$.aa","")]
	    , showPerl6ClassDef ns "Native" "NBool" [("NativeBool","$.aa","")]
	    ]
    asPerl6Object (NBit aa) = "NBit.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NInt aa) = "NInt.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NUint aa) = "NUint.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NBuf aa) = "NBuf.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NNum aa) = "NNum.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NCplx aa) = "NCplx.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NStr aa) = "NStr.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NBool aa) = "NBool.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass Native where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Native"
	    , showMooseClassDef ns "Native" "NBit" [("NativeBit","aa","")]
	    , showMooseClassDef ns "Native" "NInt" [("NativeInt","aa","")]
	    , showMooseClassDef ns "Native" "NUint" [("NativeInt","aa","")]
	    , showMooseClassDef ns "Native" "NBuf" [("NativeBuf","aa","")]
	    , showMooseClassDef ns "Native" "NNum" [("NativeNum","aa","")]
	    , showMooseClassDef ns "Native" "NCplx" [("NativeComplex","aa","")]
	    , showMooseClassDef ns "Native" "NStr" [("NativeStr","aa","")]
	    , showMooseClassDef ns "Native" "NBool" [("NativeBool","aa","")]
	    ]

instance YAML ValUndef where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"UUndef" -> do
	    return UUndef
	"UWhatever" -> do
	    return UWhatever
	"UFailure" -> do
	    let ESeq [aa] = e
	    liftM UFailure (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["UUndef","UWhatever","UFailure"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (UUndef) = asYAMLcls "UUndef"
    asYAML (UWhatever) = asYAMLcls "UWhatever"
    asYAML (UFailure aa) = asYAMLseq "UFailure" [asYAML aa]

instance Perl6Class ValUndef where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "ValUndef"
	    , showPerl6ClassDef ns "ValUndef" "UUndef" []
	    , showPerl6ClassDef ns "ValUndef" "UWhatever" []
	    , showPerl6ClassDef ns "ValUndef" "UFailure" [("ObjId","$.aa","")]
	    ]
    asPerl6Object (UUndef) = "UUndef.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (UWhatever) = "UWhatever.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (UFailure aa) = "UFailure.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass ValUndef where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "ValUndef"
	    , showMooseClassDef ns "ValUndef" "UUndef" []
	    , showMooseClassDef ns "ValUndef" "UWhatever" []
	    , showMooseClassDef ns "ValUndef" "UFailure" [("ObjId","aa","")]
	    ]

instance YAML Sign where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"SPositive" -> do
	    return SPositive
	"SNegative" -> do
	    return SNegative
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["SPositive","SNegative"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (SPositive) = asYAMLcls "SPositive"
    asYAML (SNegative) = asYAMLcls "SNegative"

instance Perl6Class Sign where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Sign"
	    , showPerl6ClassDef ns "Sign" "SPositive" []
	    , showPerl6ClassDef ns "Sign" "SNegative" []
	    ]
    asPerl6Object (SPositive) = "SPositive.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (SNegative) = "SNegative.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass Sign where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Sign"
	    , showMooseClassDef ns "Sign" "SPositive" []
	    , showMooseClassDef ns "Sign" "SNegative" []
	    ]

instance YAML NativeInt where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"IFinite" -> do
	    let ESeq [aa] = e
	    liftM IFinite (fromYAML aa)
	"IInfinite" -> do
	    let ESeq [aa] = e
	    liftM IInfinite (fromYAML aa)
	"INotANumber" -> do
	    return INotANumber
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["IFinite","IInfinite","INotANumber"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (IFinite aa) = asYAMLseq "IFinite" [asYAML aa]
    asYAML (IInfinite aa) = asYAMLseq "IInfinite" [asYAML aa]
    asYAML (INotANumber) = asYAMLcls "INotANumber"

instance Perl6Class NativeInt where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "NativeInt"
	    , showPerl6ClassDef ns "NativeInt" "IFinite" [("Integer","$.aa","")]
	    , showPerl6ClassDef ns "NativeInt" "IInfinite" [("Sign","$.aa","")]
	    , showPerl6ClassDef ns "NativeInt" "INotANumber" []
	    ]
    asPerl6Object (IFinite aa) = "IFinite.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (IInfinite aa) = "IInfinite.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (INotANumber) = "INotANumber.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass NativeInt where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "NativeInt"
	    , showMooseClassDef ns "NativeInt" "IFinite" [("Integer","aa","")]
	    , showMooseClassDef ns "NativeInt" "IInfinite" [("Sign","aa","")]
	    , showMooseClassDef ns "NativeInt" "INotANumber" []
	    ]

instance YAML NativeNum where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"NRational" -> do
	    let ESeq [aa] = e
	    liftM NRational (fromYAML aa)
	"NFloat" -> do
	    let ESeq [aa] = e
	    liftM NFloat (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["NRational","NFloat"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (NRational aa) = asYAMLseq "NRational" [asYAML aa]
    asYAML (NFloat aa) = asYAMLseq "NFloat" [asYAML aa]

instance Perl6Class NativeNum where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "NativeNum"
	    , showPerl6ClassDef ns "NativeNum" "NRational" [("Rational","$.aa","")]
	    , showPerl6ClassDef ns "NativeNum" "NFloat" [("Float","$.aa","")]
	    ]
    asPerl6Object (NRational aa) = "NRational.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (NFloat aa) = "NFloat.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass NativeNum where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "NativeNum"
	    , showMooseClassDef ns "NativeNum" "NRational" [("Rational","aa","")]
	    , showMooseClassDef ns "NativeNum" "NFloat" [("Float","aa","")]
	    ]

instance YAML NativeComplex where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkComplex" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkComplex (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkComplex"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkComplex aa ab) = asYAMLseq "MkComplex"
	   [asYAML aa, asYAML ab]

instance Perl6Class NativeComplex where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "NativeComplex"
	    , showPerl6ClassDef ns "NativeComplex" "MkComplex" [("NativeNum","$.c_real",""),("NativeNum","$.c_imaginary","")]
	    ]
    asPerl6Object MkComplex {} = error $ "not yet: " ++ "Body {constructor = \"MkComplex\", labels = [\"c_real\",\"c_imaginary\"], types = [Con \"NativeNum\",Con \"NativeNum\"]}"

instance MooseClass NativeComplex where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "NativeComplex"
	    , showMooseClassDef ns "NativeComplex" "MkComplex" [("NativeNum","c_real",""),("NativeNum","c_imaginary","")]
	    ]

instance YAML PureList where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkList" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkList (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkList"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkList aa ab) = asYAMLseq "MkList" [asYAML aa, asYAML ab]

instance Perl6Class PureList where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "PureList"
	    , showPerl6ClassDef ns "PureList" "MkList" [("PureSeq","$.l_seq",""),("PureRange","$.l_range","")]
	    ]
    asPerl6Object MkList {} = error $ "not yet: " ++ "Body {constructor = \"MkList\", labels = [\"l_seq\",\"l_range\"], types = [Con \"PureSeq\",Con \"PureRange\"]}"

instance MooseClass PureList where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "PureList"
	    , showMooseClassDef ns "PureList" "MkList" [("PureSeq","l_seq",""),("PureRange","l_range","")]
	    ]

instance YAML PureRange where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkRange" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkRange (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkRange"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkRange aa ab ac) = asYAMLseq "MkRange"
	   [asYAML aa, asYAML ab, asYAML ac]

instance Perl6Class PureRange where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "PureRange"
	    , showPerl6ClassDef ns "PureRange" "MkRange" [("Val","$.r_from",""),("Val","$.r_to",""),("Code","$.r_next","")]
	    ]
    asPerl6Object MkRange {} = error $ "not yet: " ++ "Body {constructor = \"MkRange\", labels = [\"r_from\",\"r_to\",\"r_next\"], types = [Con \"Val\",Con \"Val\",Con \"Code\"]}"

instance MooseClass PureRange where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "PureRange"
	    , showMooseClassDef ns "PureRange" "MkRange" [("Val","r_from",""),("Val","r_to",""),("Code","r_next","")]
	    ]

instance YAML MemBuf where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkBuf" -> do
	    let ESeq [aa] = e
	    liftM MkBuf (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkBuf"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkBuf aa) = asYAMLseq "MkBuf" [asYAML aa]

instance Perl6Class MemBuf where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "MemBuf"
	    , showPerl6ClassDef ns "MemBuf" "MkBuf" [("","$.b_buffer","LApply (Con \"IOUArray\") [Con \"Word64\",Con \"Word8\"]")]
	    ]
    asPerl6Object MkBuf {} = error $ "not yet: " ++ "Body {constructor = \"MkBuf\", labels = [\"b_buffer\"], types = [LApply (Con \"IOUArray\") [Con \"Word64\",Con \"Word8\"]]}"

instance MooseClass MemBuf where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "MemBuf"
	    , showMooseClassDef ns "MemBuf" "MkBuf" [("","b_buffer","LApply (Con \"IOUArray\") [Con \"Word64\",Con \"Word8\"]")]
	    ]

instance YAML PureJunc where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkJunc" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkJunc (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkJunc"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkJunc aa ab ac) = asYAMLseq "MkJunc"
	   [asYAML aa, asYAML ab, asYAML ac]

instance Perl6Class PureJunc where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "PureJunc"
	    , showPerl6ClassDef ns "PureJunc" "MkJunc" [("JuncType","$.j_type",""),("PureSet","$.j_dup",""),("PureSet","$.j_set","")]
	    ]
    asPerl6Object MkJunc {} = error $ "not yet: " ++ "Body {constructor = \"MkJunc\", labels = [\"j_type\",\"j_dup\",\"j_set\"], types = [Con \"JuncType\",Con \"PureSet\",Con \"PureSet\"]}"

instance MooseClass PureJunc where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "PureJunc"
	    , showMooseClassDef ns "PureJunc" "MkJunc" [("JuncType","j_type",""),("PureSet","j_dup",""),("PureSet","j_set","")]
	    ]

instance YAML JuncType where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"JAny" -> do
	    return JAny
	"JAll" -> do
	    return JAll
	"JNone" -> do
	    return JNone
	"JOne" -> do
	    return JOne
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["JAny","JAll","JNone","JOne"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (JAny) = asYAMLcls "JAny"
    asYAML (JAll) = asYAMLcls "JAll"
    asYAML (JNone) = asYAMLcls "JNone"
    asYAML (JOne) = asYAMLcls "JOne"

instance Perl6Class JuncType where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "JuncType"
	    , showPerl6ClassDef ns "JuncType" "JAny" []
	    , showPerl6ClassDef ns "JuncType" "JAll" []
	    , showPerl6ClassDef ns "JuncType" "JNone" []
	    , showPerl6ClassDef ns "JuncType" "JOne" []
	    ]
    asPerl6Object (JAny) = "JAny.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (JAll) = "JAll.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (JNone) = "JNone.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (JOne) = "JOne.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass JuncType where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "JuncType"
	    , showMooseClassDef ns "JuncType" "JAny" []
	    , showMooseClassDef ns "JuncType" "JAll" []
	    , showMooseClassDef ns "JuncType" "JNone" []
	    , showMooseClassDef ns "JuncType" "JOne" []
	    ]

instance YAML PurePair where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkPair" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkPair (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkPair"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkPair aa ab) = asYAMLseq "MkPair" [asYAML aa, asYAML ab]

instance Perl6Class PurePair where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "PurePair"
	    , showPerl6ClassDef ns "PurePair" "MkPair" [("Val","$.p_key",""),("Val","$.p_val","")]
	    ]
    asPerl6Object MkPair {} = error $ "not yet: " ++ "Body {constructor = \"MkPair\", labels = [\"p_key\",\"p_val\"], types = [Con \"Val\",Con \"Val\"]}"

instance MooseClass PurePair where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "PurePair"
	    , showMooseClassDef ns "PurePair" "MkPair" [("Val","p_key",""),("Val","p_val","")]
	    ]

instance YAML PureMap where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Map" -> do
	    let ESeq [aa, ab] = e
	    liftM2 Map (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Map"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Map aa ab) = asYAMLseq "Map" [asYAML aa, asYAML ab]

instance Perl6Class PureMap where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "PureMap"
	    , showPerl6ClassDef ns "PureMap" "Map" [("Val","$.aa",""),("Val","$.ab","")]
	    ]
    asPerl6Object (Map aa ab) = "Map.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"

instance MooseClass PureMap where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "PureMap"
	    , showMooseClassDef ns "PureMap" "Map" [("Val","aa",""),("Val","ab","")]
	    ]

instance YAML ValPure where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PBit" -> do
	    let ESeq [aa] = e
	    liftM PBit (fromYAML aa)
	"PInt" -> do
	    let ESeq [aa] = e
	    liftM PInt (fromYAML aa)
	"PStr" -> do
	    let ESeq [aa] = e
	    liftM PStr (fromYAML aa)
	"PNum" -> do
	    let ESeq [aa] = e
	    liftM PNum (fromYAML aa)
	"PComplex" -> do
	    let ESeq [aa] = e
	    liftM PComplex (fromYAML aa)
	"PBool" -> do
	    let ESeq [aa] = e
	    liftM PBool (fromYAML aa)
	"PException" -> do
	    let ESeq [aa] = e
	    liftM PException (fromYAML aa)
	"PCode" -> do
	    let ESeq [aa] = e
	    liftM PCode (fromYAML aa)
	"PBlock" -> do
	    let ESeq [aa] = e
	    liftM PBlock (fromYAML aa)
	"PList" -> do
	    let ESeq [aa] = e
	    liftM PList (fromYAML aa)
	"PSeq" -> do
	    let ESeq [aa] = e
	    liftM PSeq (fromYAML aa)
	"PRange" -> do
	    let ESeq [aa] = e
	    liftM PRange (fromYAML aa)
	"PSet" -> do
	    let ESeq [aa] = e
	    liftM PSet (fromYAML aa)
	"PJunc" -> do
	    let ESeq [aa] = e
	    liftM PJunc (fromYAML aa)
	"PPair" -> do
	    let ESeq [aa] = e
	    liftM PPair (fromYAML aa)
	"PMap" -> do
	    let ESeq [aa] = e
	    liftM PMap (fromYAML aa)
	"PSig" -> do
	    let ESeq [aa] = e
	    liftM PSig (fromYAML aa)
	"PCap" -> do
	    let ESeq [aa] = e
	    liftM PCap (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PBit","PInt","PStr","PNum","PComplex","PBool","PException","PCode","PBlock","PList","PSeq","PRange","PSet","PJunc","PPair","PMap","PSig","PCap"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PBit aa) = asYAMLseq "PBit" [asYAML aa]
    asYAML (PInt aa) = asYAMLseq "PInt" [asYAML aa]
    asYAML (PStr aa) = asYAMLseq "PStr" [asYAML aa]
    asYAML (PNum aa) = asYAMLseq "PNum" [asYAML aa]
    asYAML (PComplex aa) = asYAMLseq "PComplex" [asYAML aa]
    asYAML (PBool aa) = asYAMLseq "PBool" [asYAML aa]
    asYAML (PException aa) = asYAMLseq "PException" [asYAML aa]
    asYAML (PCode aa) = asYAMLseq "PCode" [asYAML aa]
    asYAML (PBlock aa) = asYAMLseq "PBlock" [asYAML aa]
    asYAML (PList aa) = asYAMLseq "PList" [asYAML aa]
    asYAML (PSeq aa) = asYAMLseq "PSeq" [asYAML aa]
    asYAML (PRange aa) = asYAMLseq "PRange" [asYAML aa]
    asYAML (PSet aa) = asYAMLseq "PSet" [asYAML aa]
    asYAML (PJunc aa) = asYAMLseq "PJunc" [asYAML aa]
    asYAML (PPair aa) = asYAMLseq "PPair" [asYAML aa]
    asYAML (PMap aa) = asYAMLseq "PMap" [asYAML aa]
    asYAML (PSig aa) = asYAMLseq "PSig" [asYAML aa]
    asYAML (PCap aa) = asYAMLseq "PCap" [asYAML aa]

instance Perl6Class ValPure where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "ValPure"
	    , showPerl6ClassDef ns "ValPure" "PBit" [("PureBit","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PInt" [("PureInt","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PStr" [("PureStr","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PNum" [("PureNum","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PComplex" [("PureComplex","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PBool" [("PureBool","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PException" [("PureException","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PCode" [("PureCode","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PBlock" [("PureCode","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PList" [("PureList","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PSeq" [("PureSeq","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PRange" [("PureRange","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PSet" [("PureSet","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PJunc" [("PureJunc","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PPair" [("PurePair","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PMap" [("PureMap","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PSig" [("PureSig","$.aa","")]
	    , showPerl6ClassDef ns "ValPure" "PCap" [("PureCap","$.aa","")]
	    ]
    asPerl6Object (PBit aa) = "PBit.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PInt aa) = "PInt.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PStr aa) = "PStr.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PNum aa) = "PNum.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PComplex aa) = "PComplex.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PBool aa) = "PBool.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PException aa) = "PException.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PCode aa) = "PCode.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PBlock aa) = "PBlock.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PList aa) = "PList.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PSeq aa) = "PSeq.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PRange aa) = "PRange.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PSet aa) = "PSet.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PJunc aa) = "PJunc.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PPair aa) = "PPair.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PMap aa) = "PMap.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PSig aa) = "PSig.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (PCap aa) = "PCap.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass ValPure where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "ValPure"
	    , showMooseClassDef ns "ValPure" "PBit" [("PureBit","aa","")]
	    , showMooseClassDef ns "ValPure" "PInt" [("PureInt","aa","")]
	    , showMooseClassDef ns "ValPure" "PStr" [("PureStr","aa","")]
	    , showMooseClassDef ns "ValPure" "PNum" [("PureNum","aa","")]
	    , showMooseClassDef ns "ValPure" "PComplex" [("PureComplex","aa","")]
	    , showMooseClassDef ns "ValPure" "PBool" [("PureBool","aa","")]
	    , showMooseClassDef ns "ValPure" "PException" [("PureException","aa","")]
	    , showMooseClassDef ns "ValPure" "PCode" [("PureCode","aa","")]
	    , showMooseClassDef ns "ValPure" "PBlock" [("PureCode","aa","")]
	    , showMooseClassDef ns "ValPure" "PList" [("PureList","aa","")]
	    , showMooseClassDef ns "ValPure" "PSeq" [("PureSeq","aa","")]
	    , showMooseClassDef ns "ValPure" "PRange" [("PureRange","aa","")]
	    , showMooseClassDef ns "ValPure" "PSet" [("PureSet","aa","")]
	    , showMooseClassDef ns "ValPure" "PJunc" [("PureJunc","aa","")]
	    , showMooseClassDef ns "ValPure" "PPair" [("PurePair","aa","")]
	    , showMooseClassDef ns "ValPure" "PMap" [("PureMap","aa","")]
	    , showMooseClassDef ns "ValPure" "PSig" [("PureSig","aa","")]
	    , showMooseClassDef ns "ValPure" "PCap" [("PureCap","aa","")]
	    ]

instance YAML Bogus where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Bogus" -> do
	    return Bogus
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Bogus"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Bogus) = asYAMLcls "Bogus"

instance Perl6Class Bogus where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Bogus"
	    , showPerl6ClassDef ns "Bogus" "Bogus" []
	    ]
    asPerl6Object (Bogus) = "Bogus.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass Bogus where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Bogus"
	    , showMooseClassDef ns "Bogus" "Bogus" []
	    ]

instance YAML ValMut where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MScalar" -> do
	    let ESeq [aa] = e
	    liftM MScalar (fromYAML aa)
	"MArray" -> do
	    let ESeq [aa] = e
	    liftM MArray (fromYAML aa)
	"MHash" -> do
	    let ESeq [aa] = e
	    liftM MHash (fromYAML aa)
	"MBuf" -> do
	    let ESeq [aa] = e
	    liftM MBuf (fromYAML aa)
	"MRoutine" -> do
	    let ESeq [aa] = e
	    liftM MRoutine (fromYAML aa)
	"MSub" -> do
	    let ESeq [aa] = e
	    liftM MSub (fromYAML aa)
	"MMethod" -> do
	    let ESeq [aa] = e
	    liftM MMethod (fromYAML aa)
	"MSubmethod" -> do
	    let ESeq [aa] = e
	    liftM MSubmethod (fromYAML aa)
	"MMacro" -> do
	    return MMacro
	"MRegex" -> do
	    let ESeq [aa] = e
	    liftM MRegex (fromYAML aa)
	"MMatch" -> do
	    let ESeq [aa] = e
	    liftM MMatch (fromYAML aa)
	"MPackage" -> do
	    let ESeq [aa] = e
	    liftM MPackage (fromYAML aa)
	"MModule" -> do
	    let ESeq [aa] = e
	    liftM MModule (fromYAML aa)
	"MClass" -> do
	    let ESeq [aa] = e
	    liftM MClass (fromYAML aa)
	"MRole" -> do
	    let ESeq [aa] = e
	    liftM MRole (fromYAML aa)
	"MGrammar" -> do
	    let ESeq [aa] = e
	    liftM MGrammar (fromYAML aa)
	"MObject" -> do
	    let ESeq [aa] = e
	    liftM MObject (fromYAML aa)
	"MForeign" -> do
	    let ESeq [aa] = e
	    liftM MForeign (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MScalar","MArray","MHash","MBuf","MRoutine","MSub","MMethod","MSubmethod","MMacro","MRegex","MMatch","MPackage","MModule","MClass","MRole","MGrammar","MObject","MForeign"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MScalar aa) = asYAMLseq "MScalar" [asYAML aa]
    asYAML (MArray aa) = asYAMLseq "MArray" [asYAML aa]
    asYAML (MHash aa) = asYAMLseq "MHash" [asYAML aa]
    asYAML (MBuf aa) = asYAMLseq "MBuf" [asYAML aa]
    asYAML (MRoutine aa) = asYAMLseq "MRoutine" [asYAML aa]
    asYAML (MSub aa) = asYAMLseq "MSub" [asYAML aa]
    asYAML (MMethod aa) = asYAMLseq "MMethod" [asYAML aa]
    asYAML (MSubmethod aa) = asYAMLseq "MSubmethod" [asYAML aa]
    asYAML (MMacro) = asYAMLcls "MMacro"
    asYAML (MRegex aa) = asYAMLseq "MRegex" [asYAML aa]
    asYAML (MMatch aa) = asYAMLseq "MMatch" [asYAML aa]
    asYAML (MPackage aa) = asYAMLseq "MPackage" [asYAML aa]
    asYAML (MModule aa) = asYAMLseq "MModule" [asYAML aa]
    asYAML (MClass aa) = asYAMLseq "MClass" [asYAML aa]
    asYAML (MRole aa) = asYAMLseq "MRole" [asYAML aa]
    asYAML (MGrammar aa) = asYAMLseq "MGrammar" [asYAML aa]
    asYAML (MObject aa) = asYAMLseq "MObject" [asYAML aa]
    asYAML (MForeign aa) = asYAMLseq "MForeign" [asYAML aa]

instance Perl6Class ValMut where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "ValMut"
	    , showPerl6ClassDef ns "ValMut" "MScalar" [("MutScalar","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MArray" [("MutArray","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MHash" [("MutHash","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MBuf" [("MutBuf","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MRoutine" [("MutRoutine","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MSub" [("MutRoutine","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MMethod" [("MutRoutine","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MSubmethod" [("MutRoutine","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MMacro" []
	    , showPerl6ClassDef ns "ValMut" "MRegex" [("MutVRule","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MMatch" [("MutVMatch","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MPackage" [("MutPackage","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MModule" [("MutModule","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MClass" [("MutClass","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MRole" [("MutRole","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MGrammar" [("MutGrammar","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MObject" [("MutObject","$.aa","")]
	    , showPerl6ClassDef ns "ValMut" "MForeign" [("MutDynamic","$.aa","")]
	    ]
    asPerl6Object (MScalar aa) = "MScalar.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MArray aa) = "MArray.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MHash aa) = "MHash.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MBuf aa) = "MBuf.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MRoutine aa) = "MRoutine.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MSub aa) = "MSub.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MMethod aa) = "MMethod.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MSubmethod aa) = "MSubmethod.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MMacro) = "MMacro.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MRegex aa) = "MRegex.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MMatch aa) = "MMatch.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MPackage aa) = "MPackage.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MModule aa) = "MModule.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MClass aa) = "MClass.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MRole aa) = "MRole.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MGrammar aa) = "MGrammar.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MObject aa) = "MObject.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (MForeign aa) = "MForeign.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass ValMut where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "ValMut"
	    , showMooseClassDef ns "ValMut" "MScalar" [("MutScalar","aa","")]
	    , showMooseClassDef ns "ValMut" "MArray" [("MutArray","aa","")]
	    , showMooseClassDef ns "ValMut" "MHash" [("MutHash","aa","")]
	    , showMooseClassDef ns "ValMut" "MBuf" [("MutBuf","aa","")]
	    , showMooseClassDef ns "ValMut" "MRoutine" [("MutRoutine","aa","")]
	    , showMooseClassDef ns "ValMut" "MSub" [("MutRoutine","aa","")]
	    , showMooseClassDef ns "ValMut" "MMethod" [("MutRoutine","aa","")]
	    , showMooseClassDef ns "ValMut" "MSubmethod" [("MutRoutine","aa","")]
	    , showMooseClassDef ns "ValMut" "MMacro" []
	    , showMooseClassDef ns "ValMut" "MRegex" [("MutVRule","aa","")]
	    , showMooseClassDef ns "ValMut" "MMatch" [("MutVMatch","aa","")]
	    , showMooseClassDef ns "ValMut" "MPackage" [("MutPackage","aa","")]
	    , showMooseClassDef ns "ValMut" "MModule" [("MutModule","aa","")]
	    , showMooseClassDef ns "ValMut" "MClass" [("MutClass","aa","")]
	    , showMooseClassDef ns "ValMut" "MRole" [("MutRole","aa","")]
	    , showMooseClassDef ns "ValMut" "MGrammar" [("MutGrammar","aa","")]
	    , showMooseClassDef ns "ValMut" "MObject" [("MutObject","aa","")]
	    , showMooseClassDef ns "ValMut" "MForeign" [("MutDynamic","aa","")]
	    ]

instance YAML IOFile where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Handle" -> do
	    return Handle
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Handle"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Handle) = asYAMLcls "Handle"

instance Perl6Class IOFile where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "IOFile"
	    , showPerl6ClassDef ns "IOFile" "Handle" []
	    ]
    asPerl6Object (Handle) = "Handle.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass IOFile where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "IOFile"
	    , showMooseClassDef ns "IOFile" "Handle" []
	    ]

instance YAML IOSocket where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Socket" -> do
	    return Socket
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Socket"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Socket) = asYAMLcls "Socket"

instance Perl6Class IOSocket where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "IOSocket"
	    , showPerl6ClassDef ns "IOSocket" "Socket" []
	    ]
    asPerl6Object (Socket) = "Socket.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass IOSocket where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "IOSocket"
	    , showMooseClassDef ns "IOSocket" "Socket" []
	    ]

instance (YAML a) => YAML (IOThread a) where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"IOThread" -> do
	    let ESeq [aa] = e
	    liftM IOThread (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["IOThread"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (IOThread aa) = asYAMLseq "IOThread" [asYAML aa]

instance (Perl6Class a) => Perl6Class (IOThread a) where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "IOThread"
	    , showPerl6ClassDef ns "IOThread" "IOThread" [("","$.aa","Var \"a\"")]
	    ]
    asPerl6Object (IOThread aa) = "IOThread.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance (MooseClass a) => MooseClass (IOThread a) where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "IOThread"
	    , showMooseClassDef ns "IOThread" "IOThread" [("","aa","Var \"a\"")]
	    ]

instance YAML IOProcess where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"ProcessHandle" -> do
	    return ProcessHandle
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["ProcessHandle"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (ProcessHandle) = asYAMLcls "ProcessHandle"

instance Perl6Class IOProcess where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "IOProcess"
	    , showPerl6ClassDef ns "IOProcess" "ProcessHandle" []
	    ]
    asPerl6Object (ProcessHandle) = "ProcessHandle.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass IOProcess where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "IOProcess"
	    , showMooseClassDef ns "IOProcess" "ProcessHandle" []
	    ]

instance YAML ValIO where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"IFile" -> do
	    let ESeq [aa] = e
	    liftM IFile (fromYAML aa)
	"ISocket" -> do
	    let ESeq [aa] = e
	    liftM ISocket (fromYAML aa)
	"IThread" -> do
	    let ESeq [aa] = e
	    liftM IThread (fromYAML aa)
	"IProcess" -> do
	    let ESeq [aa] = e
	    liftM IProcess (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["IFile","ISocket","IThread","IProcess"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (IFile aa) = asYAMLseq "IFile" [asYAML aa]
    asYAML (ISocket aa) = asYAMLseq "ISocket" [asYAML aa]
    asYAML (IThread aa) = asYAMLseq "IThread" [asYAML aa]
    asYAML (IProcess aa) = asYAMLseq "IProcess" [asYAML aa]

instance Perl6Class ValIO where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "ValIO"
	    , showPerl6ClassDef ns "ValIO" "IFile" [("IOFile","$.aa","")]
	    , showPerl6ClassDef ns "ValIO" "ISocket" [("IOSocket","$.aa","")]
	    , showPerl6ClassDef ns "ValIO" "IThread" [("","$.aa","LApply (Con \"IOThread\") [Con \"Val\"]")]
	    , showPerl6ClassDef ns "ValIO" "IProcess" [("IOProcess","$.aa","")]
	    ]
    asPerl6Object (IFile aa) = "IFile.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (ISocket aa) = "ISocket.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (IThread aa) = "IThread.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (IProcess aa) = "IProcess.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass ValIO where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "ValIO"
	    , showMooseClassDef ns "ValIO" "IFile" [("IOFile","aa","")]
	    , showMooseClassDef ns "ValIO" "ISocket" [("IOSocket","aa","")]
	    , showMooseClassDef ns "ValIO" "IThread" [("","aa","LApply (Con \"IOThread\") [Con \"Val\"]")]
	    , showMooseClassDef ns "ValIO" "IProcess" [("IOProcess","aa","")]
	    ]

instance YAML SubType where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"SubMethod" -> do
	    return SubMethod
	"SubCoroutine" -> do
	    return SubCoroutine
	"SubMacro" -> do
	    return SubMacro
	"SubRoutine" -> do
	    return SubRoutine
	"SubBlock" -> do
	    return SubBlock
	"SubPointy" -> do
	    return SubPointy
	"SubPrim" -> do
	    return SubPrim
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["SubMethod","SubCoroutine","SubMacro","SubRoutine","SubBlock","SubPointy","SubPrim"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (SubMethod) = asYAMLcls "SubMethod"
    asYAML (SubCoroutine) = asYAMLcls "SubCoroutine"
    asYAML (SubMacro) = asYAMLcls "SubMacro"
    asYAML (SubRoutine) = asYAMLcls "SubRoutine"
    asYAML (SubBlock) = asYAMLcls "SubBlock"
    asYAML (SubPointy) = asYAMLcls "SubPointy"
    asYAML (SubPrim) = asYAMLcls "SubPrim"

instance YAML Stmt where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkStmt" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkStmt (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkStmt"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkStmt aa ab ac) = asYAMLseq "MkStmt"
	   [asYAML aa, asYAML ab, asYAML ac]

instance Perl6Class Stmt where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Stmt"
	    , showPerl6ClassDef ns "Stmt" "MkStmt" [("Ident","$.label",""),("Table","$.pragmas",""),("Exp","$.expression","")]
	    ]
    asPerl6Object MkStmt {} = error $ "not yet: " ++ "Body {constructor = \"MkStmt\", labels = [\"label\",\"pragmas\",\"expression\"], types = [LApply (Con \"Maybe\") [Con \"Ident\"],Con \"Table\",Con \"Exp\"]}"

instance MooseClass Stmt where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Stmt"
	    , showMooseClassDef ns "Stmt" "MkStmt" [("Ident","label",""),("Table","pragmas",""),("Exp","expression","")]
	    ]

instance YAML Exp where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"ENoop" -> do
	    return ENoop
	"EVar" -> do
	    let ESeq [aa] = e
	    liftM EVar (fromYAML aa)
	"EVal" -> do
	    let ESeq [aa] = e
	    liftM EVal (fromYAML aa)
	"EDeref" -> do
	    let ESeq [aa] = e
	    liftM EDeref (fromYAML aa)
	"EBind" -> do
	    let ESeq [aa, ab] = e
	    liftM2 EBind (fromYAML aa) (fromYAML ab)
	"EAssign" -> do
	    let ESeq [aa, ab] = e
	    liftM2 EAssign (fromYAML aa) (fromYAML ab)
	"EControl" -> do
	    let ESeq [aa] = e
	    liftM EControl (fromYAML aa)
	"EFlatten" -> do
	    let ESeq [aa] = e
	    liftM EFlatten (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["ENoop","EVar","EVal","EDeref","EBind","EAssign","EControl","EFlatten"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (ENoop) = asYAMLcls "ENoop"
    asYAML (EVar aa) = asYAMLseq "EVar" [asYAML aa]
    asYAML (EVal aa) = asYAMLseq "EVal" [asYAML aa]
    asYAML (EDeref aa) = asYAMLseq "EDeref" [asYAML aa]
    asYAML (EBind aa ab) = asYAMLseq "EBind" [asYAML aa, asYAML ab]
    asYAML (EAssign aa ab) = asYAMLseq "EAssign" [asYAML aa, asYAML ab]
    asYAML (EControl aa) = asYAMLseq "EControl" [asYAML aa]
    asYAML (EFlatten aa) = asYAMLseq "EFlatten" [asYAML aa]

instance Perl6Class Exp where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Exp"
	    , showPerl6ClassDef ns "Exp" "ENoop" []
	    , showPerl6ClassDef ns "Exp" "EVar" [("ExpVar","$.aa","")]
	    , showPerl6ClassDef ns "Exp" "EVal" [("ExpVal","$.aa","")]
	    , showPerl6ClassDef ns "Exp" "EDeref" [("ExpVar","$.aa","")]
	    , showPerl6ClassDef ns "Exp" "EBind" [("Exp","$.aa",""),("Exp","$.ab","")]
	    , showPerl6ClassDef ns "Exp" "EAssign" [("Exp","$.aa",""),("Exp","$.ab","")]
	    , showPerl6ClassDef ns "Exp" "EControl" [("ExpControl","$.aa","")]
	    , showPerl6ClassDef ns "Exp" "EFlatten" [("Exp","@.aa","")]
	    ]
    asPerl6Object (ENoop) = "ENoop.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (EVar aa) = "EVar.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (EVal aa) = "EVal.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (EDeref aa) = "EDeref.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (EBind aa ab) = "EBind.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (EAssign aa ab) = "EAssign.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (EControl aa) = "EControl.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (EFlatten aa) = "EFlatten.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"

instance MooseClass Exp where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Exp"
	    , showMooseClassDef ns "Exp" "ENoop" []
	    , showMooseClassDef ns "Exp" "EVar" [("ExpVar","aa","")]
	    , showMooseClassDef ns "Exp" "EVal" [("ExpVal","aa","")]
	    , showMooseClassDef ns "Exp" "EDeref" [("ExpVar","aa","")]
	    , showMooseClassDef ns "Exp" "EBind" [("Exp","aa",""),("Exp","ab","")]
	    , showMooseClassDef ns "Exp" "EAssign" [("Exp","aa",""),("Exp","ab","")]
	    , showMooseClassDef ns "Exp" "EControl" [("ExpControl","aa","")]
	    , showMooseClassDef ns "Exp" "EFlatten" [("ArrayRef","aa","List (Con \"Exp\")")]
	    ]

instance YAML ExpControl where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"CCall" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CCall (fromYAML aa) (fromYAML ab)
	"CApply" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CApply (fromYAML aa) (fromYAML ab)
	"CCond" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CCond (fromYAML aa) (fromYAML ab)
	"CTrenaryCond" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 CTrenaryCond (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"CCondBlock" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 CCondBlock (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"CGoto" -> do
	    let ESeq [aa] = e
	    liftM CGoto (fromYAML aa)
	"CWhile" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CWhile (fromYAML aa) (fromYAML ab)
	"CGiven" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CGiven (fromYAML aa) (fromYAML ab)
	"CWhen" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CWhen (fromYAML aa) (fromYAML ab)
	"CForeign" -> do
	    return CForeign
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["CCall","CApply","CCond","CTrenaryCond","CCondBlock","CGoto","CWhile","CGiven","CWhen","CForeign"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (CCall aa ab) = asYAMLseq "CCall" [asYAML aa, asYAML ab]
    asYAML (CApply aa ab) = asYAMLseq "CApply" [asYAML aa, asYAML ab]
    asYAML (CCond aa ab) = asYAMLseq "CCond" [asYAML aa, asYAML ab]
    asYAML (CTrenaryCond aa ab ac) = asYAMLseq "CTrenaryCond"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (CCondBlock aa ab ac) = asYAMLseq "CCondBlock"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (CGoto aa) = asYAMLseq "CGoto" [asYAML aa]
    asYAML (CWhile aa ab) = asYAMLseq "CWhile" [asYAML aa, asYAML ab]
    asYAML (CGiven aa ab) = asYAMLseq "CGiven" [asYAML aa, asYAML ab]
    asYAML (CWhen aa ab) = asYAMLseq "CWhen" [asYAML aa, asYAML ab]
    asYAML (CForeign) = asYAMLcls "CForeign"

instance Perl6Class ExpControl where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "ExpControl"
	    , showPerl6ClassDef ns "ExpControl" "CCall" [("Ident","$.aa",""),("Cap","$.ab","")]
	    , showPerl6ClassDef ns "ExpControl" "CApply" [("Exp","$.aa",""),("Cap","$.ab","")]
	    , showPerl6ClassDef ns "ExpControl" "CCond" [("Exp","$.aa",""),("Code","$.ab","")]
	    , showPerl6ClassDef ns "ExpControl" "CTrenaryCond" [("Exp","$.aa",""),("Code","$.ab",""),("Code","$.ac","")]
	    , showPerl6ClassDef ns "ExpControl" "CCondBlock" [("","$.aa","Tuple [Con \"Exp\",Con \"Code\"]"),("","@.ab","List (Tuple [Con \"Exp\",Con \"Code\"])"),("Code","$.ac","")]
	    , showPerl6ClassDef ns "ExpControl" "CGoto" [("Ident","$.aa","")]
	    , showPerl6ClassDef ns "ExpControl" "CWhile" [("Exp","$.aa",""),("Code","$.ab","")]
	    , showPerl6ClassDef ns "ExpControl" "CGiven" [("Exp","$.aa",""),("Code","$.ab","")]
	    , showPerl6ClassDef ns "ExpControl" "CWhen" [("Exp","$.aa",""),("Code","$.ab","")]
	    , showPerl6ClassDef ns "ExpControl" "CForeign" []
	    ]
    asPerl6Object (CCall aa ab) = "CCall.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (CApply aa ab) = "CApply.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (CCond aa ab) = "CCond.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (CTrenaryCond aa ab ac) = "CTrenaryCond.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab, plShow ac]) ++ ")"
    asPerl6Object (CCondBlock aa ab ac) = "CCondBlock.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab, plShow ac]) ++ ")"
    asPerl6Object (CGoto aa) = "CGoto.new(" ++ (concat $ intersperse ", " [plShow aa]) ++ ")"
    asPerl6Object (CWhile aa ab) = "CWhile.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (CGiven aa ab) = "CGiven.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (CWhen aa ab) = "CWhen.new(" ++ (concat $ intersperse ", " [plShow aa, plShow ab]) ++ ")"
    asPerl6Object (CForeign) = "CForeign.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass ExpControl where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "ExpControl"
	    , showMooseClassDef ns "ExpControl" "CCall" [("Ident","aa",""),("Cap","ab","")]
	    , showMooseClassDef ns "ExpControl" "CApply" [("Exp","aa",""),("Cap","ab","")]
	    , showMooseClassDef ns "ExpControl" "CCond" [("Exp","aa",""),("Code","ab","")]
	    , showMooseClassDef ns "ExpControl" "CTrenaryCond" [("Exp","aa",""),("Code","ab",""),("Code","ac","")]
	    , showMooseClassDef ns "ExpControl" "CCondBlock" [("","aa","Tuple [Con \"Exp\",Con \"Code\"]"),("ArrayRef","ab","List (Tuple [Con \"Exp\",Con \"Code\"])"),("Code","ac","")]
	    , showMooseClassDef ns "ExpControl" "CGoto" [("Ident","aa","")]
	    , showMooseClassDef ns "ExpControl" "CWhile" [("Exp","aa",""),("Code","ab","")]
	    , showMooseClassDef ns "ExpControl" "CGiven" [("Exp","aa",""),("Code","ab","")]
	    , showMooseClassDef ns "ExpControl" "CWhen" [("Exp","aa",""),("Code","ab","")]
	    , showMooseClassDef ns "ExpControl" "CForeign" []
	    ]

instance YAML Param where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkParam" -> do
	    let liftM10 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; x10 <- m10; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)}
	    let ESeq [aa, ab, ac, ad, ae, af, ag, ah, ai, aj] = e
	    liftM10 MkParam (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af) (fromYAML ag) (fromYAML ah) (fromYAML ai) (fromYAML aj)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkParam"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkParam aa ab ac ad ae af ag ah ai aj) =
	   asYAMLseq "MkParam"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af,
	    asYAML ag, asYAML ah, asYAML ai, asYAML aj]

instance Perl6Class Param where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Param"
	    , showPerl6ClassDef ns "Param" "MkParam" [("Ident","$.p_variable",""),("Class","@.p_types",""),("Code","@.p_constraints",""),("Sig","$.p_unpacking",""),("Exp","$.p_default",""),("Ident","$.p_label",""),("Table","$.p_slots",""),("ParamAccess","$.p_hasAccess",""),("Bool","$.p_isRef",""),("Bool","$.p_isLazy","")]
	    ]
    asPerl6Object MkParam {} = error $ "not yet: " ++ "Body {constructor = \"MkParam\", labels = [\"p_variable\",\"p_types\",\"p_constraints\",\"p_unpacking\",\"p_default\",\"p_label\",\"p_slots\",\"p_hasAccess\",\"p_isRef\",\"p_isLazy\"], types = [Con \"Ident\",List (Con \"Class\"),List (Con \"Code\"),LApply (Con \"Maybe\") [Con \"Sig\"],LApply (Con \"Maybe\") [Con \"Exp\"],Con \"Ident\",Con \"Table\",Con \"ParamAccess\",Con \"Bool\",Con \"Bool\"]}"

instance MooseClass Param where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Param"
	    , showMooseClassDef ns "Param" "MkParam" [("Ident","p_variable",""),("ArrayRef","p_types","List (Con \"Class\")"),("ArrayRef","p_constraints","List (Con \"Code\")"),("Sig","p_unpacking",""),("Exp","p_default",""),("Ident","p_label",""),("Table","p_slots",""),("ParamAccess","p_hasAccess",""),("Bool","p_isRef",""),("Bool","p_isLazy","")]
	    ]

instance YAML ParamAccess where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"AccRO" -> do
	    return AccRO
	"AccRW" -> do
	    return AccRW
	"AccCopy" -> do
	    return AccCopy
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["AccRO","AccRW","AccCopy"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (AccRO) = asYAMLcls "AccRO"
    asYAML (AccRW) = asYAMLcls "AccRW"
    asYAML (AccCopy) = asYAMLcls "AccCopy"

instance Perl6Class ParamAccess where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "ParamAccess"
	    , showPerl6ClassDef ns "ParamAccess" "AccRO" []
	    , showPerl6ClassDef ns "ParamAccess" "AccRW" []
	    , showPerl6ClassDef ns "ParamAccess" "AccCopy" []
	    ]
    asPerl6Object (AccRO) = "AccRO.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (AccRW) = "AccRW.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (AccCopy) = "AccCopy.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass ParamAccess where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "ParamAccess"
	    , showMooseClassDef ns "ParamAccess" "AccRO" []
	    , showMooseClassDef ns "ParamAccess" "AccRW" []
	    , showMooseClassDef ns "ParamAccess" "AccCopy" []
	    ]

instance YAML CodeAssoc where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"AssLeft" -> do
	    return AssLeft
	"AssRight" -> do
	    return AssRight
	"AssNon" -> do
	    return AssNon
	"AssChain" -> do
	    return AssChain
	"AssList" -> do
	    return AssList
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["AssLeft","AssRight","AssNon","AssChain","AssList"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (AssLeft) = asYAMLcls "AssLeft"
    asYAML (AssRight) = asYAMLcls "AssRight"
    asYAML (AssNon) = asYAMLcls "AssNon"
    asYAML (AssChain) = asYAMLcls "AssChain"
    asYAML (AssList) = asYAMLcls "AssList"

instance Perl6Class CodeAssoc where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "CodeAssoc"
	    , showPerl6ClassDef ns "CodeAssoc" "AssLeft" []
	    , showPerl6ClassDef ns "CodeAssoc" "AssRight" []
	    , showPerl6ClassDef ns "CodeAssoc" "AssNon" []
	    , showPerl6ClassDef ns "CodeAssoc" "AssChain" []
	    , showPerl6ClassDef ns "CodeAssoc" "AssList" []
	    ]
    asPerl6Object (AssLeft) = "AssLeft.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (AssRight) = "AssRight.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (AssNon) = "AssNon.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (AssChain) = "AssChain.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (AssList) = "AssList.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass CodeAssoc where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "CodeAssoc"
	    , showMooseClassDef ns "CodeAssoc" "AssLeft" []
	    , showMooseClassDef ns "CodeAssoc" "AssRight" []
	    , showMooseClassDef ns "CodeAssoc" "AssNon" []
	    , showMooseClassDef ns "CodeAssoc" "AssChain" []
	    , showMooseClassDef ns "CodeAssoc" "AssList" []
	    ]

instance YAML Sig where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"SigMethSingle" -> do
	    let liftM10 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; x10 <- m10; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)}
	    let ESeq [aa, ab, ac, ad, ae, af, ag, ah, ai, aj] = e
	    liftM10 SigMethSingle (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af) (fromYAML ag) (fromYAML ah) (fromYAML ai) (fromYAML aj)
	"SigSubSingle" -> do
	    let liftM9 f m1 m2 m3 m4 m5 m6 m7 m8 m9 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9)}
	    let ESeq [aa, ab, ac, ad, ae, af, ag, ah, ai] = e
	    liftM9 SigSubSingle (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af) (fromYAML ag) (fromYAML ah) (fromYAML ai)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["SigMethSingle","SigSubSingle"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (SigMethSingle aa ab ac ad ae af ag ah ai aj) =
	   asYAMLseq "SigMethSingle"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af,
	    asYAML ag, asYAML ah, asYAML ai, asYAML aj]
    asYAML (SigSubSingle aa ab ac ad ae af ag ah ai) =
	   asYAMLseq "SigSubSingle"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af,
	    asYAML ag, asYAML ah, asYAML ai]

instance Perl6Class Sig where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Sig"
	    , showPerl6ClassDef ns "Sig" "SigMethSingle" [("Param","$.s_invocant",""),("Int","$.s_requiredPositionalCount",""),("","$.s_requiredNames","LApply (Con \"Set\") [Con \"Ident\"]"),("Param","@.s_positionalList",""),("","$.s_namedSet","LApply (Con \"Map\") [Con \"Ident\",Con \"Param\"]"),("Param","@.s_slurpyScalarList",""),("Param","$.s_slurpyArray",""),("Param","$.s_slurpyHash",""),("Param","$.s_slurpyCode",""),("Param","$.s_slurpyCapture","")]
	    , showPerl6ClassDef ns "Sig" "SigSubSingle" [("Int","$.s_requiredPositionalCount",""),("","$.s_requiredNames","LApply (Con \"Set\") [Con \"Ident\"]"),("Param","@.s_positionalList",""),("","$.s_namedSet","LApply (Con \"Map\") [Con \"Ident\",Con \"Param\"]"),("Param","@.s_slurpyScalarList",""),("Param","$.s_slurpyArray",""),("Param","$.s_slurpyHash",""),("Param","$.s_slurpyCode",""),("Param","$.s_slurpyCapture","")]
	    ]
    asPerl6Object SigMethSingle {} = error $ "not yet: " ++ "Body {constructor = \"SigMethSingle\", labels = [\"s_invocant\",\"s_requiredPositionalCount\",\"s_requiredNames\",\"s_positionalList\",\"s_namedSet\",\"s_slurpyScalarList\",\"s_slurpyArray\",\"s_slurpyHash\",\"s_slurpyCode\",\"s_slurpyCapture\"], types = [Con \"Param\",Con \"Int\",LApply (Con \"Set\") [Con \"Ident\"],List (Con \"Param\"),LApply (Con \"Map\") [Con \"Ident\",Con \"Param\"],List (Con \"Param\"),LApply (Con \"Maybe\") [Con \"Param\"],LApply (Con \"Maybe\") [Con \"Param\"],LApply (Con \"Maybe\") [Con \"Param\"],LApply (Con \"Maybe\") [Con \"Param\"]]}"
    asPerl6Object SigSubSingle {} = error $ "not yet: " ++ "Body {constructor = \"SigSubSingle\", labels = [\"s_requiredPositionalCount\",\"s_requiredNames\",\"s_positionalList\",\"s_namedSet\",\"s_slurpyScalarList\",\"s_slurpyArray\",\"s_slurpyHash\",\"s_slurpyCode\",\"s_slurpyCapture\"], types = [Con \"Int\",LApply (Con \"Set\") [Con \"Ident\"],List (Con \"Param\"),LApply (Con \"Map\") [Con \"Ident\",Con \"Param\"],List (Con \"Param\"),LApply (Con \"Maybe\") [Con \"Param\"],LApply (Con \"Maybe\") [Con \"Param\"],LApply (Con \"Maybe\") [Con \"Param\"],LApply (Con \"Maybe\") [Con \"Param\"]]}"

instance MooseClass Sig where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Sig"
	    , showMooseClassDef ns "Sig" "SigMethSingle" [("Param","s_invocant",""),("Int","s_requiredPositionalCount",""),("","s_requiredNames","LApply (Con \"Set\") [Con \"Ident\"]"),("ArrayRef","s_positionalList","List (Con \"Param\")"),("","s_namedSet","LApply (Con \"Map\") [Con \"Ident\",Con \"Param\"]"),("ArrayRef","s_slurpyScalarList","List (Con \"Param\")"),("Param","s_slurpyArray",""),("Param","s_slurpyHash",""),("Param","s_slurpyCode",""),("Param","s_slurpyCapture","")]
	    , showMooseClassDef ns "Sig" "SigSubSingle" [("Int","s_requiredPositionalCount",""),("","s_requiredNames","LApply (Con \"Set\") [Con \"Ident\"]"),("ArrayRef","s_positionalList","List (Con \"Param\")"),("","s_namedSet","LApply (Con \"Map\") [Con \"Ident\",Con \"Param\"]"),("ArrayRef","s_slurpyScalarList","List (Con \"Param\")"),("Param","s_slurpyArray",""),("Param","s_slurpyHash",""),("Param","s_slurpyCode",""),("Param","s_slurpyCapture","")]
	    ]

instance YAML Routine where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"RoutineSimple" -> do
	    let ESeq [aa, ab] = e
	    liftM2 RoutineSimple (fromYAML aa) (fromYAML ab)
	"RoutineMulti" -> do
	    let ESeq [aa, ab] = e
	    liftM2 RoutineMulti (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["RoutineSimple","RoutineMulti"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (RoutineSimple aa ab) = asYAMLseq "RoutineSimple"
	   [asYAML aa, asYAML ab]
    asYAML (RoutineMulti aa ab) = asYAMLseq "RoutineMulti"
	   [asYAML aa, asYAML ab]

instance Perl6Class Routine where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Routine"
	    , showPerl6ClassDef ns "Routine" "RoutineSimple" [("CodeWrapping","$.wrappings",""),("Code","$.routineCode","")]
	    , showPerl6ClassDef ns "Routine" "RoutineMulti" [("CodeWrapping","$.wrappings",""),("","$.routineVariants","LApply (Con \"Set\") [Con \"MultiVariant\"]")]
	    ]
    asPerl6Object RoutineSimple {} = error $ "not yet: " ++ "Body {constructor = \"RoutineSimple\", labels = [\"wrappings\",\"routineCode\"], types = [Con \"CodeWrapping\",Con \"Code\"]}"
    asPerl6Object RoutineMulti {} = error $ "not yet: " ++ "Body {constructor = \"RoutineMulti\", labels = [\"wrappings\",\"routineVariants\"], types = [Con \"CodeWrapping\",LApply (Con \"Set\") [Con \"MultiVariant\"]]}"

instance MooseClass Routine where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Routine"
	    , showMooseClassDef ns "Routine" "RoutineSimple" [("CodeWrapping","wrappings",""),("Code","routineCode","")]
	    , showMooseClassDef ns "Routine" "RoutineMulti" [("CodeWrapping","wrappings",""),("","routineVariants","LApply (Con \"Set\") [Con \"MultiVariant\"]")]
	    ]

instance YAML Code where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"CodePerl" -> do
	    let liftM16 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; x10 <- m10; x11 <- m11; x12 <- m12; x13 <- m13; x14 <- m14; x15 <- m15; x16 <- m16; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)}
	    let ESeq [aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap] = e
	    liftM16 CodePerl (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af) (fromYAML ag) (fromYAML ah) (fromYAML ai) (fromYAML aj) (fromYAML ak) (fromYAML al) (fromYAML am) (fromYAML an) (fromYAML ao) (fromYAML ap)
	"CodePrim" -> do
	    let ESeq [aa, ab, ac, ad, ae] = e
	    liftM5 CodePrim (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["CodePerl","CodePrim"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (CodePerl aa ab ac ad ae af ag ah ai aj ak al am an ao ap) =
	   asYAMLseq "CodePerl"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af,
	    asYAML ag, asYAML ah, asYAML ai, asYAML aj, asYAML ak, asYAML al,
	    asYAML am, asYAML an, asYAML ao, asYAML ap]
    asYAML (CodePrim aa ab ac ad ae) = asYAMLseq "CodePrim"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae]

instance Perl6Class Code where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Code"
	    , showPerl6ClassDef ns "Code" "CodePerl" [("Sig","$.c_signature",""),("Rational","$.c_precedence",""),("CodeAssoc","$.c_assoc",""),("Bool","$.c_isRW",""),("Bool","$.c_isSafe",""),("Bool","$.c_isCached",""),("Stmt","@.c_body",""),("Pad","$.c_pad",""),("Table","$.c_traits",""),("Code","@.c_preBlocks",""),("Code","@.c_postBlocks",""),("Code","@.c_firstBlocks",""),("Code","@.c_lastBlocks",""),("Code","@.c_nextBlocks",""),("Code","@.c_keepBlocks",""),("Code","@.c_undoBlocks","")]
	    , showPerl6ClassDef ns "Code" "CodePrim" [("Sig","$.c_signature",""),("Rational","$.c_precedence",""),("CodeAssoc","$.c_assoc",""),("Bool","$.c_isRW",""),("Bool","$.c_isSafe","")]
	    ]
    asPerl6Object CodePerl {} = error $ "not yet: " ++ "Body {constructor = \"CodePerl\", labels = [\"c_signature\",\"c_precedence\",\"c_assoc\",\"c_isRW\",\"c_isSafe\",\"c_isCached\",\"c_body\",\"c_pad\",\"c_traits\",\"c_preBlocks\",\"c_postBlocks\",\"c_firstBlocks\",\"c_lastBlocks\",\"c_nextBlocks\",\"c_keepBlocks\",\"c_undoBlocks\"], types = [Con \"Sig\",Con \"Rational\",Con \"CodeAssoc\",Con \"Bool\",Con \"Bool\",Con \"Bool\",List (Con \"Stmt\"),Con \"Pad\",Con \"Table\",List (Con \"Code\"),List (Con \"Code\"),List (Con \"Code\"),List (Con \"Code\"),List (Con \"Code\"),List (Con \"Code\"),List (Con \"Code\")]}"
    asPerl6Object CodePrim {} = error $ "not yet: " ++ "Body {constructor = \"CodePrim\", labels = [\"c_signature\",\"c_precedence\",\"c_assoc\",\"c_isRW\",\"c_isSafe\"], types = [Con \"Sig\",Con \"Rational\",Con \"CodeAssoc\",Con \"Bool\",Con \"Bool\"]}"

instance MooseClass Code where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Code"
	    , showMooseClassDef ns "Code" "CodePerl" [("Sig","c_signature",""),("Rational","c_precedence",""),("CodeAssoc","c_assoc",""),("Bool","c_isRW",""),("Bool","c_isSafe",""),("Bool","c_isCached",""),("ArrayRef","c_body","List (Con \"Stmt\")"),("Pad","c_pad",""),("Table","c_traits",""),("ArrayRef","c_preBlocks","List (Con \"Code\")"),("ArrayRef","c_postBlocks","List (Con \"Code\")"),("ArrayRef","c_firstBlocks","List (Con \"Code\")"),("ArrayRef","c_lastBlocks","List (Con \"Code\")"),("ArrayRef","c_nextBlocks","List (Con \"Code\")"),("ArrayRef","c_keepBlocks","List (Con \"Code\")"),("ArrayRef","c_undoBlocks","List (Con \"Code\")")]
	    , showMooseClassDef ns "Code" "CodePrim" [("Sig","c_signature",""),("Rational","c_precedence",""),("CodeAssoc","c_assoc",""),("Bool","c_isRW",""),("Bool","c_isSafe","")]
	    ]

instance YAML MultiVariant where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkMultiVariant" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkMultiVariant (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkMultiVariant"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkMultiVariant aa ab ac) = asYAMLseq "MkMultiVariant"
	   [asYAML aa, asYAML ab, asYAML ac]

instance Perl6Class MultiVariant where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "MultiVariant"
	    , showPerl6ClassDef ns "MultiVariant" "MkMultiVariant" [("IntSet","$.m_semicolonOffsets",""),("Code","$.m_callable",""),("CodeWrapping","$.m_extraWrappings","")]
	    ]
    asPerl6Object MkMultiVariant {} = error $ "not yet: " ++ "Body {constructor = \"MkMultiVariant\", labels = [\"m_semicolonOffsets\",\"m_callable\",\"m_extraWrappings\"], types = [Con \"IntSet\",Con \"Code\",LApply (Con \"Maybe\") [Con \"CodeWrapping\"]]}"

instance MooseClass MultiVariant where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "MultiVariant"
	    , showMooseClassDef ns "MultiVariant" "MkMultiVariant" [("IntSet","m_semicolonOffsets",""),("Code","m_callable",""),("CodeWrapping","m_extraWrappings","")]
	    ]

instance YAML EntryDeclarator where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"DeclMy" -> do
	    return DeclMy
	"DeclOur" -> do
	    return DeclOur
	"DeclHas" -> do
	    return DeclHas
	"DeclState" -> do
	    return DeclState
	"DeclConstant" -> do
	    return DeclConstant
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["DeclMy","DeclOur","DeclHas","DeclState","DeclConstant"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (DeclMy) = asYAMLcls "DeclMy"
    asYAML (DeclOur) = asYAMLcls "DeclOur"
    asYAML (DeclHas) = asYAMLcls "DeclHas"
    asYAML (DeclState) = asYAMLcls "DeclState"
    asYAML (DeclConstant) = asYAMLcls "DeclConstant"

instance Perl6Class EntryDeclarator where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "EntryDeclarator"
	    , showPerl6ClassDef ns "EntryDeclarator" "DeclMy" []
	    , showPerl6ClassDef ns "EntryDeclarator" "DeclOur" []
	    , showPerl6ClassDef ns "EntryDeclarator" "DeclHas" []
	    , showPerl6ClassDef ns "EntryDeclarator" "DeclState" []
	    , showPerl6ClassDef ns "EntryDeclarator" "DeclConstant" []
	    ]
    asPerl6Object (DeclMy) = "DeclMy.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (DeclOur) = "DeclOur.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (DeclHas) = "DeclHas.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (DeclState) = "DeclState.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (DeclConstant) = "DeclConstant.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass EntryDeclarator where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "EntryDeclarator"
	    , showMooseClassDef ns "EntryDeclarator" "DeclMy" []
	    , showMooseClassDef ns "EntryDeclarator" "DeclOur" []
	    , showMooseClassDef ns "EntryDeclarator" "DeclHas" []
	    , showMooseClassDef ns "EntryDeclarator" "DeclState" []
	    , showMooseClassDef ns "EntryDeclarator" "DeclConstant" []
	    ]

instance YAML PadEntry where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkEntry" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkEntry (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkEntry"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkEntry aa ab) = asYAMLseq "MkEntry" [asYAML aa, asYAML ab]

instance Perl6Class PadEntry where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "PadEntry"
	    , showPerl6ClassDef ns "PadEntry" "MkEntry" [("EntryDeclarator","$.e_declarator",""),("EntryStorage","$.e_storage","")]
	    ]
    asPerl6Object MkEntry {} = error $ "not yet: " ++ "Body {constructor = \"MkEntry\", labels = [\"e_declarator\",\"e_storage\"], types = [Con \"EntryDeclarator\",Con \"EntryStorage\"]}"

instance MooseClass PadEntry where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "PadEntry"
	    , showMooseClassDef ns "PadEntry" "MkEntry" [("EntryDeclarator","e_declarator",""),("EntryStorage","e_storage","")]
	    ]

instance YAML MutObject where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"ObjInstance" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 ObjInstance (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"MkForeign" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkForeign (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"MkPrototype" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkPrototype (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["ObjInstance","MkForeign","MkPrototype"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (ObjInstance aa ab ac) = asYAMLseq "ObjInstance"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (MkForeign aa ab ac) = asYAMLseq "MkForeign"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (MkPrototype aa ab) = asYAMLseq "MkPrototype"
	   [asYAML aa, asYAML ab]

instance Perl6Class MutObject where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "MutObject"
	    , showPerl6ClassDef ns "MutObject" "ObjInstance" [("ObjId","$.o_id",""),("ObjClass","$.o_meta",""),("ObjSlots","$.o_slots","")]
	    , showPerl6ClassDef ns "MutObject" "MkForeign" [("ObjId","$.o_id",""),("ObjClass","$.o_meta",""),("ObjPayload","$.o_payload","")]
	    , showPerl6ClassDef ns "MutObject" "MkPrototype" [("ObjId","$.o_id",""),("ObjClass","$.o_meta","")]
	    ]
    asPerl6Object ObjInstance {} = error $ "not yet: " ++ "Body {constructor = \"ObjInstance\", labels = [\"o_id\",\"o_meta\",\"o_slots\"], types = [Con \"ObjId\",Con \"ObjClass\",Con \"ObjSlots\"]}"
    asPerl6Object MkForeign {} = error $ "not yet: " ++ "Body {constructor = \"MkForeign\", labels = [\"o_id\",\"o_meta\",\"o_payload\"], types = [Con \"ObjId\",Con \"ObjClass\",Con \"ObjPayload\"]}"
    asPerl6Object MkPrototype {} = error $ "not yet: " ++ "Body {constructor = \"MkPrototype\", labels = [\"o_id\",\"o_meta\"], types = [Con \"ObjId\",Con \"ObjClass\"]}"

instance MooseClass MutObject where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "MutObject"
	    , showMooseClassDef ns "MutObject" "ObjInstance" [("ObjId","o_id",""),("ObjClass","o_meta",""),("ObjSlots","o_slots","")]
	    , showMooseClassDef ns "MutObject" "MkForeign" [("ObjId","o_id",""),("ObjClass","o_meta",""),("ObjPayload","o_payload","")]
	    , showMooseClassDef ns "MutObject" "MkPrototype" [("ObjId","o_id",""),("ObjClass","o_meta","")]
	    ]

instance YAML Cap where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"CaptMeth" -> do
	    let ESeq [aa, ab] = e
	    liftM2 CaptMeth (fromYAML aa) (fromYAML ab)
	"CaptSub" -> do
	    let ESeq [aa] = e
	    liftM CaptSub (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["CaptMeth","CaptSub"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (CaptMeth aa ab) = asYAMLseq "CaptMeth"
	   [asYAML aa, asYAML ab]
    asYAML (CaptSub aa) = asYAMLseq "CaptSub" [asYAML aa]

instance Perl6Class Cap where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Cap"
	    , showPerl6ClassDef ns "Cap" "CaptMeth" [("Exp","$.c_invocant",""),("Arglist","@.c_argstack","")]
	    , showPerl6ClassDef ns "Cap" "CaptSub" [("Arglist","@.c_argstack","")]
	    ]
    asPerl6Object CaptMeth {} = error $ "not yet: " ++ "Body {constructor = \"CaptMeth\", labels = [\"c_invocant\",\"c_argstack\"], types = [Con \"Exp\",List (Con \"Arglist\")]}"
    asPerl6Object CaptSub {} = error $ "not yet: " ++ "Body {constructor = \"CaptSub\", labels = [\"c_argstack\"], types = [List (Con \"Arglist\")]}"

instance MooseClass Cap where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Cap"
	    , showMooseClassDef ns "Cap" "CaptMeth" [("Exp","c_invocant",""),("ArrayRef","c_argstack","List (Con \"Arglist\")")]
	    , showMooseClassDef ns "Cap" "CaptSub" [("ArrayRef","c_argstack","List (Con \"Arglist\")")]
	    ]

instance YAML Arglist where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkArglist" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkArglist (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkArglist"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkArglist aa ab) = asYAMLseq "MkArglist"
	   [asYAML aa, asYAML ab]

instance Perl6Class Arglist where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Arglist"
	    , showPerl6ClassDef ns "Arglist" "MkArglist" [("Exp","@.a_positional",""),("","$.a_named","LApply (Con \"Map\") [Con \"Str\",List (Con \"Exp\")]")]
	    ]
    asPerl6Object MkArglist {} = error $ "not yet: " ++ "Body {constructor = \"MkArglist\", labels = [\"a_positional\",\"a_named\"], types = [List (Con \"Exp\"),LApply (Con \"Map\") [Con \"Str\",List (Con \"Exp\")]]}"

instance MooseClass Arglist where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Arglist"
	    , showMooseClassDef ns "Arglist" "MkArglist" [("ArrayRef","a_positional","List (Con \"Exp\")"),("","a_named","LApply (Con \"Map\") [Con \"Str\",List (Con \"Exp\")]")]
	    ]

instance YAML Var where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"VarLexical" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 VarLexical (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"VarDynamic" -> do
	    let ESeq [aa, ab] = e
	    liftM2 VarDynamic (fromYAML aa) (fromYAML ab)
	"VarMagic" -> do
	    let ESeq [aa] = e
	    liftM VarMagic (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["VarLexical","VarDynamic","VarMagic"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (VarLexical aa ab ac) = asYAMLseq "VarLexical"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (VarDynamic aa ab) = asYAMLseq "VarDynamic"
	   [asYAML aa, asYAML ab]
    asYAML (VarMagic aa) = asYAMLseq "VarMagic" [asYAML aa]

instance Perl6Class Var where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Var"
	    , showPerl6ClassDef ns "Var" "VarLexical" [("Ident","$.v_name",""),("Int","$.v_callerCount",""),("Int","$.v_outerCount","")]
	    , showPerl6ClassDef ns "Var" "VarDynamic" [("Ident","$.v_name",""),("Ident","@.v_packageName","")]
	    , showPerl6ClassDef ns "Var" "VarMagic" [("Magic","$.v_magic","")]
	    ]
    asPerl6Object VarLexical {} = error $ "not yet: " ++ "Body {constructor = \"VarLexical\", labels = [\"v_name\",\"v_callerCount\",\"v_outerCount\"], types = [Con \"Ident\",Con \"Int\",Con \"Int\"]}"
    asPerl6Object VarDynamic {} = error $ "not yet: " ++ "Body {constructor = \"VarDynamic\", labels = [\"v_name\",\"v_packageName\"], types = [Con \"Ident\",List (Con \"Ident\")]}"
    asPerl6Object VarMagic {} = error $ "not yet: " ++ "Body {constructor = \"VarMagic\", labels = [\"v_magic\"], types = [Con \"Magic\"]}"

instance MooseClass Var where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Var"
	    , showMooseClassDef ns "Var" "VarLexical" [("Ident","v_name",""),("Int","v_callerCount",""),("Int","v_outerCount","")]
	    , showMooseClassDef ns "Var" "VarDynamic" [("Ident","v_name",""),("ArrayRef","v_packageName","List (Con \"Ident\")")]
	    , showMooseClassDef ns "Var" "VarMagic" [("Magic","v_magic","")]
	    ]

instance YAML Magic where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MOS" -> do
	    return MOS
	"MOSVer" -> do
	    return MOSVer
	"MPerlVer" -> do
	    return MPerlVer
	"MFile" -> do
	    return MFile
	"MLine" -> do
	    return MLine
	"MScalarPackage" -> do
	    return MScalarPackage
	"MArrayPackages" -> do
	    return MArrayPackages
	"MScalarModule" -> do
	    return MScalarModule
	"MArrayModules" -> do
	    return MArrayModules
	"MScalarClass" -> do
	    return MScalarClass
	"MArrayClasses" -> do
	    return MArrayClasses
	"MScalarRole" -> do
	    return MScalarRole
	"MArrayRoles" -> do
	    return MArrayRoles
	"MScalarGrammar" -> do
	    return MScalarGrammar
	"MArrayGrammars" -> do
	    return MArrayGrammars
	"MParser" -> do
	    return MParser
	"MScalarRoutine" -> do
	    return MScalarRoutine
	"MArrayRoutines" -> do
	    return MArrayRoutines
	"MScalarBlock" -> do
	    return MScalarBlock
	"MArrayBlocks" -> do
	    return MArrayBlocks
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MOS","MOSVer","MPerlVer","MFile","MLine","MScalarPackage","MArrayPackages","MScalarModule","MArrayModules","MScalarClass","MArrayClasses","MScalarRole","MArrayRoles","MScalarGrammar","MArrayGrammars","MParser","MScalarRoutine","MArrayRoutines","MScalarBlock","MArrayBlocks"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MOS) = asYAMLcls "MOS"
    asYAML (MOSVer) = asYAMLcls "MOSVer"
    asYAML (MPerlVer) = asYAMLcls "MPerlVer"
    asYAML (MFile) = asYAMLcls "MFile"
    asYAML (MLine) = asYAMLcls "MLine"
    asYAML (MScalarPackage) = asYAMLcls "MScalarPackage"
    asYAML (MArrayPackages) = asYAMLcls "MArrayPackages"
    asYAML (MScalarModule) = asYAMLcls "MScalarModule"
    asYAML (MArrayModules) = asYAMLcls "MArrayModules"
    asYAML (MScalarClass) = asYAMLcls "MScalarClass"
    asYAML (MArrayClasses) = asYAMLcls "MArrayClasses"
    asYAML (MScalarRole) = asYAMLcls "MScalarRole"
    asYAML (MArrayRoles) = asYAMLcls "MArrayRoles"
    asYAML (MScalarGrammar) = asYAMLcls "MScalarGrammar"
    asYAML (MArrayGrammars) = asYAMLcls "MArrayGrammars"
    asYAML (MParser) = asYAMLcls "MParser"
    asYAML (MScalarRoutine) = asYAMLcls "MScalarRoutine"
    asYAML (MArrayRoutines) = asYAMLcls "MArrayRoutines"
    asYAML (MScalarBlock) = asYAMLcls "MScalarBlock"
    asYAML (MArrayBlocks) = asYAMLcls "MArrayBlocks"

instance Perl6Class Magic where
    showPerl6TypeDef ns _ = unlines
	    [ showPerl6RoleDef ns "Magic"
	    , showPerl6ClassDef ns "Magic" "MOS" []
	    , showPerl6ClassDef ns "Magic" "MOSVer" []
	    , showPerl6ClassDef ns "Magic" "MPerlVer" []
	    , showPerl6ClassDef ns "Magic" "MFile" []
	    , showPerl6ClassDef ns "Magic" "MLine" []
	    , showPerl6ClassDef ns "Magic" "MScalarPackage" []
	    , showPerl6ClassDef ns "Magic" "MArrayPackages" []
	    , showPerl6ClassDef ns "Magic" "MScalarModule" []
	    , showPerl6ClassDef ns "Magic" "MArrayModules" []
	    , showPerl6ClassDef ns "Magic" "MScalarClass" []
	    , showPerl6ClassDef ns "Magic" "MArrayClasses" []
	    , showPerl6ClassDef ns "Magic" "MScalarRole" []
	    , showPerl6ClassDef ns "Magic" "MArrayRoles" []
	    , showPerl6ClassDef ns "Magic" "MScalarGrammar" []
	    , showPerl6ClassDef ns "Magic" "MArrayGrammars" []
	    , showPerl6ClassDef ns "Magic" "MParser" []
	    , showPerl6ClassDef ns "Magic" "MScalarRoutine" []
	    , showPerl6ClassDef ns "Magic" "MArrayRoutines" []
	    , showPerl6ClassDef ns "Magic" "MScalarBlock" []
	    , showPerl6ClassDef ns "Magic" "MArrayBlocks" []
	    ]
    asPerl6Object (MOS) = "MOS.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MOSVer) = "MOSVer.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MPerlVer) = "MPerlVer.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MFile) = "MFile.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MLine) = "MLine.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarPackage) = "MScalarPackage.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayPackages) = "MArrayPackages.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarModule) = "MScalarModule.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayModules) = "MArrayModules.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarClass) = "MScalarClass.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayClasses) = "MArrayClasses.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarRole) = "MScalarRole.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayRoles) = "MArrayRoles.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarGrammar) = "MScalarGrammar.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayGrammars) = "MArrayGrammars.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MParser) = "MParser.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarRoutine) = "MScalarRoutine.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayRoutines) = "MArrayRoutines.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MScalarBlock) = "MScalarBlock.new(" ++ (concat $ intersperse ", " []) ++ ")"
    asPerl6Object (MArrayBlocks) = "MArrayBlocks.new(" ++ (concat $ intersperse ", " []) ++ ")"

instance MooseClass Magic where
    showMooseTypeDef ns _ = unlines
	    [ showMooseRoleDef ns "Magic"
	    , showMooseClassDef ns "Magic" "MOS" []
	    , showMooseClassDef ns "Magic" "MOSVer" []
	    , showMooseClassDef ns "Magic" "MPerlVer" []
	    , showMooseClassDef ns "Magic" "MFile" []
	    , showMooseClassDef ns "Magic" "MLine" []
	    , showMooseClassDef ns "Magic" "MScalarPackage" []
	    , showMooseClassDef ns "Magic" "MArrayPackages" []
	    , showMooseClassDef ns "Magic" "MScalarModule" []
	    , showMooseClassDef ns "Magic" "MArrayModules" []
	    , showMooseClassDef ns "Magic" "MScalarClass" []
	    , showMooseClassDef ns "Magic" "MArrayClasses" []
	    , showMooseClassDef ns "Magic" "MScalarRole" []
	    , showMooseClassDef ns "Magic" "MArrayRoles" []
	    , showMooseClassDef ns "Magic" "MScalarGrammar" []
	    , showMooseClassDef ns "Magic" "MArrayGrammars" []
	    , showMooseClassDef ns "Magic" "MParser" []
	    , showMooseClassDef ns "Magic" "MScalarRoutine" []
	    , showMooseClassDef ns "Magic" "MArrayRoutines" []
	    , showMooseClassDef ns "Magic" "MScalarBlock" []
	    , showMooseClassDef ns "Magic" "MArrayBlocks" []
	    ]

--  Imported from other files :-


#endif
