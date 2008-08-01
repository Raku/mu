{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances -fparr #-}
module Pugs.AST.Types where
import Pugs.Internals
import Pugs.Types
import qualified Data.Set       as Set
import qualified Data.Map       as Map

import qualified Data.HashTable    as H

import Pugs.AST.Eval
import Pugs.AST.Utils
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import {-# SOURCE #-} Pugs.AST.Internals (IVar, VRef, Val, Env, Exp)


-- | Uses Haskell's underlying representation for threads.
data VThread = MkThread
    { threadId      :: ThreadId
    , threadLock    :: TMVar Val
    }
    deriving (Show, Eq, Ord, Typeable)

data VSubst
    = MkSubst
        { substRegex    :: !VRule
        , substExp      :: !Exp
        }
    | MkTrans
        { transFrom     :: !VStr
        , transTo       :: !VStr
        }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data VThunk = MkThunk
    { thunkExp  :: Eval Val
    , thunkType :: Type
    }
    deriving (Typeable) {-!derive: YAML_Pos!-}

newtype VProcess = MkProcess (ProcessHandle)
    deriving (Typeable) {-!derive: YAML_Pos!-}

type VPair = (Val, Val)

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
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

type VBlock = Exp
data VControl
    = ControlExit  !ExitCode
    | ControlContinuation
        { ccEnv     :: !Env
        , ccVal     :: !Val
        , ccCont    :: !(Val -> Eval Val)
        }
    | ControlLoop  !ControlLoop
    | ControlWhen  !ControlWhen
    | ControlLeave
        { leaveType     :: !(SubType -> Bool)
        , leaveDepth    :: !Int
        , leaveValue    :: !Val
        }
-- \| ControlLeave !(Env -> Eval Bool) !Val
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

data ControlLoop
    = LoopNext
    | LoopRedo
    | LoopLast
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

data ControlWhen
    = WhenContinue
    | WhenBreak
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

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
             | SubPointy    -- ^ Pointy block
             | SubPrim      -- ^ Built-in primitive operator (see "Pugs.Prim")
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, JSON, Perl5!-}

{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual parameter 
values, which are henceforth termed "arguments".
-}
data Param = MkOldParam -- "Old" because Pugs.Val.Code defined a new one
    { isInvocant    :: !Bool        -- ^ Is it in invocant slot?
    , isOptional    :: !Bool        -- ^ Is it optional?
    , isNamed       :: !Bool        -- ^ Is it named-only?
    , isLValue      :: !Bool        -- ^ Is it lvalue (i.e. not `is copy`)?
    , isWritable    :: !Bool        -- ^ Is it writable (i.e. `is rw`)?
    , isLazy        :: !Bool        -- ^ Is it call-by-name (short-circuit)?
    , paramName     :: !Var         -- ^ Parameter name
    , paramContext  :: !Cxt         -- ^ Parameter context: slurpiness and type
    , paramDefault  :: !Exp         -- ^ Default expression (to evaluate to)
                                    --     when omitted
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

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

data SubAssoc
    = ANil | AIrrelevantToParsing | A_left | A_right | A_non | A_chain | A_list 
    deriving (Show, Eq, Ord, Typeable, Data) {-!derive: YAML_Pos, JSON, Perl5 !-}

data MPad = MkMPad { mp_id :: !Word, mp_pad :: !(TVar Pad) }
    deriving (Show, Typeable, Data) {-!derive: YAML_Pos, JSON, Perl5 !-}

-- | Represents a sub, method, closure etc. -- basically anything callable.
data VCode = MkCode
    { isMulti           :: !Bool                  -- ^ Is this a multi sub\/method?
    , subName           :: !ByteString            -- ^ Name of the closure
    , subType           :: !SubType               -- ^ Type of the closure
    , subOuterPads      :: !LexPads               -- ^ Lexical pads for this scope
    , subInnerPad       :: !Pad                   -- ^ Inner lexical pad (immutable)
--  , subLexical        :: !Pad                   -- ^ Cached merged pads
    , subPackage        :: !Pkg                   -- ^ Package of the subroutine
    , subAssoc          :: !SubAssoc              -- ^ Associativity
    , subParams         :: !Params                -- ^ Parameters list
    , subBindings       :: !Bindings              -- ^ Currently assumed bindings
    , subSlurpLimit     :: !SlurpLimit            -- ^ Max. number of slurpy arguments
    , subReturns        :: !Type                  -- ^ Return type
    , subLValue         :: !Bool                  -- ^ Is this a lvalue sub?
    , subBody           :: !Exp                   -- ^ Body of the closure
    , subCont           :: !(Maybe (TVar VThunk)) -- ^ Coroutine re-entry point
    , subStarted        :: !(Maybe (TVar Bool))   -- ^ Whether START was run
    , subTraitBlocks    :: !TraitBlocks
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data TraitBlocks = MkTraitBlocks
    { subPreBlocks      :: ![VCode]
    , subPostBlocks     :: ![VCode]
    , subFirstBlocks    :: ![VCode]
    , subLastBlocks     :: ![VCode]
    , subNextBlocks     :: ![VCode]
    , subKeepBlocks     :: ![VCode]
    , subUndoBlocks     :: ![VCode]
    , subEnterBlocks    :: ![VCode]
    , subLeaveBlocks    :: ![VCode]
    , subControlBlocks  :: ![VCode]
    , subCatchBlocks    :: ![VCode]
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

{- Expression annotation
-}
data Ann
    = Cxt !Cxt                -- ^ Context
    | Pos !Pos                -- ^ Position
    | Prag ![Pragma]          -- ^ Lexical pragmas
    | Decl !Scope             -- ^ Within an declarator
    | Parens                  -- ^ Parenthesized
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

{- Expressions
   "App" represents function application, e.g. myfun($invocant: $arg)

   "Syn" represents a structure that cannot be represented by an App.
   For example, Syn "block" [...block body...]
                Syn "="     [lhs, rhs]
   ... or class definitions, where traits may be assigned either in
   the signature or inside the body.

   There is no top-level marker, like unix filesystems don't have
   volume letters.
-}

type DebugInfo = Maybe (TVar (Map ID String))

type LexPads = [LexPad]
data LexPad
    = PRuntime      { pr_pad :: !Pad }
    | PCompiling    { pc_pad :: !MPad }
    deriving (Show, Eq, Ord, Typeable)

data Frame
    = FrameLoop
    | FrameWhen
    | FrameGather
    | FrameRoutine
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

data IHashEnv = MkHashEnv deriving (Show, Typeable) {-!derive: YAML_Pos!-}
data IScalarCwd = MkScalarCwd deriving (Show, Typeable) {-!derive: YAML_Pos!-}

-- | A '$/' object, the return of a rx match operation.
data VMatch = MkMatch
    { matchOk           :: !VBool   -- success?
    , matchFrom         :: !Int     -- .from
    , matchTo           :: !Int     -- .to
    , matchStr          :: !VStr    -- captured str
    , matchSubPos       :: ![Val]   -- positional submatches
    , matchSubNamed     :: !(Map VStr Val)   -- named submatches
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- | type for a function introducing a change to a Pad
type PadMutator = (Pad -> Pad)

{-|
Serializable compilation unit

See: docs/notes/precompilation_cache.pod
-}
data CompUnit = MkCompUnit
    { cu_ver  :: !Int        -- a version number, see compUnitVersion
    , cu_desc :: !String     -- e.g., the name of the contained module
    , cu_pad  :: !Pad        -- pad for unit Env
    , cu_ast  :: !Exp        -- AST of unit
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos !-}

newtype IArray = MkIArray (TVar [:IVar VScalar:])
    deriving (Typeable)

type IArraySlice        = [IVar VScalar]
type IHash              = H.HashTable VStr (IVar VScalar) -- XXX UTF8 handled at Types/Hash.hs
type IScalar            = TVar Val
type IScalarProxy       = (Eval VScalar, (VScalar -> Eval ()))
type IScalarLazy        = Maybe VScalar
type IPairHashSlice     = (VStr, IVar VScalar)

data VMultiCode = MkMultiCode
    { mc_type       :: !Type
    , mc_subtype    :: !SubType
    , mc_assoc      :: !SubAssoc
    , mc_signature  :: !Params
    , mc_variants   :: !(Set Var)
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- these implementation allows no destructions
type IRule   = VRule
type IHandle = VHandle -- XXX maybe TVar?

type VScalar = Val

{-|
Module initialization information.

When a module is loaded and initialized (i.e., its &import routine is
called), it may need to communicate information back to the parser. 
This information is held in a TVar to which the parser has access.
Currently we use this for keeping track of lexical pragma change
requests, but the possiblyExit mechanism may be refactored to use
this as well.
-}
newtype InitDat = MkInitDat
    { initPragmas :: [Pragma]            -- ^ Pragma values being installed
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

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

The @Bool@ is a \'freshness\' flag used to ensure that @my@ variable slots
are re-generated each time we enter their scope; see the
'Pugs.Eval.reduce' entry for ('Pad' 'SMy' ...).

The current global and lexical pads are stored in the current 'Env', which
is stored in the @Reader@-monad component of the current 'Eval' monad.
-}

newtype Pad = MkPad { padEntries :: Map Var PadEntry }
    deriving (Eq, Ord, Typeable)

newtype EntryFlags = MkEntryFlags { ef_isContext :: Bool }
    deriving (Show, Eq, Ord, Typeable)

instance Monoid EntryFlags where
    mempty = MkEntryFlags False
    mappend (MkEntryFlags x) (MkEntryFlags y) = MkEntryFlags (x || y)

instance Show Regex where
    show _ = "<regex>"

instance Ord Regex where
    compare x y = compare (addressOf x) (addressOf y)

instance Eq Regex where
    x == y = addressOf x == addressOf y

-- Haddock doesn't seem to like data/instance declarations with a where clause.
instance Eq IHash where
    x == y = addressOf x == addressOf y
instance Ord IHash where
    compare x y = compare (addressOf x) (addressOf y)
instance Show IHash where
    show = showAddressOf "Hash"
instance Typeable2 H.HashTable where
    typeOf2 _ = mkTyConApp (mkTyCon "HashTable") []

instance Eq (IVar a) where
    x == y = addressOf x == addressOf y
instance Ord (IVar a) where
    compare x y = compare (addressOf x) (addressOf y)
instance Ord (TVar a) where
    compare x y = compare (addressOf x) (addressOf y)
instance Ord (IORef a) where
    compare x y = compare (addressOf x) (addressOf y)

instance Monoid SubAssoc where
    mempty = ANil
    mappend ANil y = y
    mappend x    _ = x

instance Eq MPad where
    x == y = mp_id x == mp_id y

instance Ord MPad where
    x `compare` y = mp_id x `compare` mp_id y

instance Ord VComplex where
    compare (a :+ ai) (b :+ bi) = compare (a, ai) (b, bi)

instance Show (TVar a) where
    show = showAddressOf "ref"

instance Show (IORef a) where
    show = showAddressOf "ref"

instance Show JuncType where
    show JAny  = "any"
    show JAll  = "all"
    show JNone = "none"
    show JOne  = "one"

instance Show VJunc where
    show (MkJunc jtype _ set) =
        (show jtype) ++ "(" ++
            (foldl (\x y ->
                if x == "" then show y
                else x ++ "," ++ show y)
            "" $ Set.elems set) ++ ")"

instance Show VThunk where
    show _ = "<thunk>"
instance Eq VThunk
instance Ord VThunk where
    compare _ _ = EQ

instance Show VProcess where
    show _ = "<process>"
instance Eq VProcess
instance Ord VProcess where
    compare _ _ = EQ

instance Typeable Unique where typeOf _ = mkTyConApp (mkTyCon "Unique") []
instance Typeable ProcessHandle where typeOf _ = mkTyConApp (mkTyCon "ProcessHandle") []
instance Typeable Regex where typeOf _ = mkTyConApp (mkTyCon "Regex") []


instance Eq VJunc where
    (MkJunc aa ab ac) == (MkJunc aa' ab' ac') = aa == aa' && ab == ab'
                      && ac == ac'

instance Ord VJunc where
    compare (MkJunc aa ab ac) (MkJunc aa' ab' ac') =
            foldl (\x y -> if x == EQ then compare y EQ else x) EQ
            [compare aa aa',compare ab ab',compare ac ac']

{-|
Transform a pad into a flat list of bindings. The inverse of 'mkPad'.

Note that @Data.Map.assocs@ returns a list of mappings in ascending key order.
-}
padToList :: Pad -> [(Var, PadEntry)]
padToList (MkPad pad) = Map.assocs pad

instance Show Pad where
    show pad = "MkPad (padToList " ++ show (padToList pad) ++ ")"

data PadEntry
    = PELexical  { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags, pe_store :: !(TVar VRef) } -- pe_fresh :: !(TVar Bool) }
    | PEStatic   { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags, pe_store :: !(TVar VRef) }
    | PEConstant { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}
