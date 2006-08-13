{- hopefully, in 6.6, this can be a first-class compilation unit. For now,
 - it's #included from Pugs.Val.
{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Code where
import Pugs.Internals
import Pugs.Types

import {-# SOURCE #-} Pugs.Val
--import {-# SOURCE #-} Pugs.Val.Sig
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
        , c_preBlocks         :: [Code]    -- ^ DBC hooks: pre(\$args --> Bool) 
        , c_postBlocks        :: [Code]
        , c_enterBlocks       :: [Code]    -- ^ AOPish hooks
        , c_leaveBlocks       :: [CodeLeave]
        , c_firstBlocks       :: [Code]
        , c_lastBlocks        :: [Code]
        , c_nextBlocks        :: [Code]
        , c_catchBlock        :: Maybe Code
        , c_controlBlock      :: Maybe Code
        }
    | CodePrim
        { c_signature         :: Sig
        , c_precedence        :: Rational
        , c_assoc             :: CodeAssoc
        , c_isRW              :: Bool
        , c_isSafe            :: Bool
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Block exit traits may be interleaved, so tag them by type
data CodeLeave
    = LeaveNormal Code        -- ^ LEAVE block
    | LeaveKeep   Code        -- ^ KEEP block
    | LeaveUndo   Code        -- ^ UNDO block
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Function associtivity
data CodeAssoc
    = AssLeft
    | AssRight
    | AssNon
    | AssChain
    | AssList
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

--------------------------------------------------------------------------------------

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

type PureSig = Sig

-- | Single parameter for a function/method, e.g.:
--   Elk $m where { $m.antlers ~~ Velvet }
{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual argument
values.
-}
data Param = MkParam
    { p_variable    :: Ident         -- ^ E.g. $m above
    , p_types       :: [Types.Type]  -- ^ Static pieces of inferencer-food
                                     --   E.g. Elk above
    , p_constraints :: [Code]        -- ^ Dynamic pieces of runtime-mood
                                     --   E.g. where {...} above
    , p_unpacking   :: Maybe PureSig -- ^ E.g. BinTree $t (Left $l, Right $r)
    , p_default     :: Maybe Exp     -- ^ E.g. $answer? = 42
    , p_label       :: Ident         -- ^ E.g. :mode
    , p_slots       :: Table         -- ^ Any additional attrib not
                                     --   explicitly mentioned below
    , p_hasAccess   :: ParamAccess   -- ^ is ro, is rw, is copy
    , p_isRef       :: Bool
    , p_isLazy      :: Bool
    } deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

data ParamAccess
    = AccessRO
    | AccessRW
    | AccessCopy
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

--------------------------------------------------------------------------------------

-- | a Capture is a frozen version of the arguments to an application.
data Capt a
    = CaptMeth
        { c_invocant :: a
        , c_argstack :: [Arglist a]
        }
    | CaptSub
        { c_argstack :: [Arglist a]
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | non-invocant arguments.
data Arglist a = MkArglist
    { a_positional :: [a]
    , a_named      :: Map Ident [a]   -- ^ maps to [a] and not a since if the Sig stipulates
                                      --   @x, "x => 1, x => 2" constructs @x = (1, 2).
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


-- | Runtime Capture with dynamic Exp for leaves
type ExpCapt = Capt Exp
-- | Static Capture with Val for leaves
type ValCapt = Capt Val

type Exp = () -- XXX bogus
