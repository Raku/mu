{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances -fparr #-}

module Pugs.AST.Val where

import Pugs.Internals
import Pugs.Types
import qualified Data.Set       as Set
import qualified Data.Map       as Map

import qualified Data.HashTable    as H
import GHC.Conc (unsafeIOToSTM)

import Pugs.Cont (callCC)
import Pugs.Parser.Number
import Pugs.AST.Eval
import Pugs.AST.Utils
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5
import qualified Pugs.Val as Val
import GHC.PArr
import {-# SOURCE #-} Pugs.AST
import {-# SOURCE #-} Pugs.AST.Internals (Exp, Env, IVar)

{-|
Represents a value.

Note that 'Val' is also a constructor for 'Exp' (i.e. an expression containing 
a value), so don't confuse the two. Similarly, all the constructors for 
@data 'Val'@ are themselves puns on the types of values they contain.
-}
data Val
    = VUndef                 -- ^ Undefined value
    | VBool     !VBool       -- ^ Boolean value
    | VInt      !VInt        -- ^ Integer value
    | VRat      !VRat        -- ^ Rational number value
    | VNum      !VNum        -- ^ Number (i.e. a double)
    | VComplex  !VComplex    -- ^ Complex number value
    | VStr      !VStr        -- ^ String value
    | VList     !VList       -- ^ List value
    | VType     !VType       -- ^ Type value (e.g. @Int@ or @Type@)
    | VJunc     !VJunc       -- ^ Junction value
    | VError    !Val ![Pos]  -- ^ Error
    | VControl  !VControl
-------------------------------------------------------------------
-- The following are runtime-only values (VRef is negotiable)
    | VRef      !VRef        -- ^ Reference value
    | VCode     !VCode       -- ^ A code object
    | VBlock    !VBlock
    | VHandle   !VHandle     -- ^ File handle
    | VSocket   !VSocket     -- ^ Socket handle
    | VThread   !VThread
    | VProcess  !VProcess    -- ^ PID value
    | VRule     !VRule       -- ^ Rule\/regex value
    | VSubst    !VSubst      -- ^ Substitution value (correct?)
    | VMatch    !VMatch      -- ^ Match value
    | VObject   !VObject     -- ^ Object
    | VOpaque   !VOpaque
    | PerlSV    !PerlSV
    | VV        !Val.Val
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

type VList = [Val]
type VType = Type
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

-- GADTs, here we come!
data VRef = forall a. Typeable a => !(IVar a)

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

-- | Uses Haskell's underlying representation for threads.
data VThread = MkThread
    { threadId      :: ThreadId
    , threadLock    :: TMVar Val
    }
    deriving (Show, Eq, Ord, Typeable)

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
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

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

data VObject = MkObject
    { objType   :: !VType
    , objAttrs  :: !IHash
    , objOpaque :: !(Maybe Dynamic)
    , objId     :: !ObjectId
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- | A '$/' object, the return of a rx match operation.
data VMatch = MkMatch
    { matchOk           :: !VBool   -- success?
    , matchFrom         :: !Int     -- .from
    , matchTo           :: !Int     -- .to
    , matchStr          :: !VStr    -- captured str
    , matchSubPos       :: !VList   -- positional submatches
    , matchSubNamed     :: !VHash   -- named submatches
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data VOpaque

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

type LexPads = [LexPad]
data LexPad
    = PRuntime      { pr_pad :: !Pad }
    | PCompiling    { pc_pad :: !MPad }
    deriving (Show, Eq, Ord, Typeable)

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

data SubAssoc
    = ANil | AIrrelevantToParsing | A_left | A_right | A_non | A_chain | A_list 
    deriving (Show, Eq, Ord, Typeable, Data) {-!derive: YAML_Pos, JSON, Perl5 !-}

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

data VThunk = MkThunk
    { thunkExp  :: Eval Val
    , thunkType :: VType
    }
    deriving (Typeable) {-!derive: YAML_Pos!-}

type IHash              = H.HashTable VStr (IVar VScalar) -- XXX UTF8 handled at Types/Hash.hs
newtype ObjectId = MkObjectId { unObjectId :: Int }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

type VHash = Map VStr Val


data MPad = MkMPad { mp_id :: !Word, mp_pad :: !(TVar Pad) }
    deriving (Show, Typeable, Data) {-!derive: YAML_Pos, JSON, Perl5 !-}

data PadEntry
    = PELexical  { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags, pe_store :: !(TVar VRef) } -- pe_fresh :: !(TVar Bool) }
    | PEStatic   { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags, pe_store :: !(TVar VRef) }
    | PEConstant { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

type VScalar = Val

newtype EntryFlags = MkEntryFlags { ef_isContext :: Bool }
    deriving (Show, Eq, Ord, Typeable)

instance Eq VRef where
    x == y = addressOf x == addressOf y
instance Ord VRef where
    compare x y = compare (addressOf x) (addressOf y)
