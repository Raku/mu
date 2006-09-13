{-
 - {-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
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
        , c_body              :: CodeBody  -- ^ AST of "do" block
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
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Block exit traits may be interleaved, so tag them by type
data CodeLeave
    = LeaveNormal Code        -- ^ LEAVE block
    | LeaveKeep   Code        -- ^ KEEP block
    | LeaveUndo   Code        -- ^ UNDO block
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Function associtivity
data CodeAssoc
    = AssLeft
    | AssRight
    | AssNon
    | AssChain
    | AssList
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

--------------------------------------------------------------------------------------

-- | AST for function signature. Separated to method and function variants
--   for ease of pattern matching.
data Sig
    = SigMethSingle
        { s_invocant                  :: Param
        , s_requiredPositionalCount   :: Int
        , s_requiredNames             :: Set ID
        , s_positionalList            :: [Param]
        , s_namedSet                  :: Map.Map ID Param
        , s_slurpyScalarList          :: [Param]
        , s_slurpyArray               :: Maybe Param
        , s_slurpyHash                :: Maybe Param
        , s_slurpyCode                :: Maybe Param
        , s_slurpyCapture             :: Maybe Param
        }
    | SigSubSingle
        { s_requiredPositionalCount   :: Int
        , s_requiredNames             :: Set ID
        , s_positionalList            :: [Param]
        , s_namedSet                  :: Map.Map ID Param
        , s_slurpyScalarList          :: [Param]
        , s_slurpyArray               :: Maybe Param
        , s_slurpyHash                :: Maybe Param
        , s_slurpyCode                :: Maybe Param
        , s_slurpyCapture             :: Maybe Param
        }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type PureSig = Sig

-- | Single parameter for a function/method, e.g.:
--   Elk $m where { $m.antlers ~~ Velvet }
{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual argument
values.
-}
data SigParam = MkParam
    { p_variable    :: Var           -- ^ E.g. $m above
    , p_types       :: [Types.Type]  -- ^ Static pieces of inferencer-food
                                     --   E.g. Elk above
    , p_constraints :: [Code]        -- ^ Dynamic pieces of runtime-mood
                                     --   E.g. where {...} above
    , p_unpacking   :: Maybe PureSig -- ^ E.g. BinTree $t (Left $l, Right $r)
    , p_default     :: ParamDefault  -- ^ E.g. $answer? = 42
    , p_label       :: ID            -- ^ The external name for the param ('m' above)
    , p_slots       :: Table         -- ^ Any additional attrib not
                                     --   explicitly mentioned below
    , p_hasAccess   :: ParamAccess   -- ^ is ro, is rw, is copy
    , p_isRef       :: Bool          -- ^ must be true if hasAccess = AccessRW
    , p_isContext   :: Bool          -- ^ "is context"
    , p_isLazy      :: Bool
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

type Param = SigParam -- to get around name clashes in Pugs.AST :(

newtype CodeBody = MkCodeBody [Stmt]
    deriving (Typeable)

newtype ParamDefault = MkParamDefault { unDefault :: Maybe Exp }
    deriving (Typeable)

instance Eq ParamDefault where _ == _ = True
instance Ord ParamDefault where compare _ _ = EQ
instance Show ParamDefault where
    show MkParamDefault{ unDefault = Nothing } = "<ParamDefault:Nothing>"
    show _    = "<ParamDefault:Just<Exp>>"

instance Eq CodeBody where _ == _ = True
instance Ord CodeBody where compare _ _ = EQ
instance Show CodeBody where show _ = "<Code.Body>"

data ParamAccess
    = AccessRO
    | AccessRW
    | AccessCopy
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

instance ICoercible P Sig where
	asStr = return . cast . render . purePretty

instance Pure Sig where
    purePretty s = colon <> (parens $ prettySig s)
    
prettySig :: Sig -> Doc
prettySig s@(SigMethSingle {}) = invocant <> colon `invSpace` (prettySubSig s)
    where
    invocant = if (v_name $ p_variable $ s_invocant s) == nullID then text "$ " else prettyParam (s_invocant s) True True
    invSpace :: Doc -> Doc -> Doc
    invSpace = if (isEmpty $ prettySubSig s) then (<>) else (<+>)
prettySig s = prettySubSig s

prettySubSig :: Sig -> Doc
prettySubSig s = sep $ punctuate comma $ concat [posParams, namedParams]
    where
    posParams = [prettyParam p r True | p <- (s_positionalList s) | r <- (replicate (s_requiredPositionalCount s) True) ++ repeat False]
    namedParams = [prettyParam p (isReqNamed n) False | (n, p) <- Map.toList $ s_namedSet s]
    isReqNamed n = Set.member n $ s_requiredNames s

prettyParam :: Param -> Bool -> Bool -> Doc
prettyParam p isReq isPos = staticTypes <+> varName <> defaultHint <+> sep
    [ traits, unpacking, constraints, debugDump ]
    where
    varName
        | isPos = text (cast $ p_variable p)
        | v_name (p_variable p) == p_label p = text $ ":" ++ (cast $ p_variable p)
        | otherwise = text ":" <> text (cast p_label p) <> (parens $ text (cast p_variable p))
    -- staticTypes = hsep $ map (text . (cast :: Types.Type -> String)) $ p_types p XXX: why is this wrong?
    staticTypes = hsep $ map (text . show) $ p_types p
    defaultHint = if not isReq && not haveDefault then text "?" else empty
    haveDefault = isJust $ unDefault $ p_default p
    defaultVal  = if haveDefault then equals <+> text "..." else empty
    traits      = sep [acc, ref, lazy, slots]
    unpacking   = case p_unpacking p of
        (Just s)   -> purePretty s
        _          -> empty
    acc         = case p_hasAccess p of
        AccessRO   -> empty
        AccessRW   -> text "is rw"
        AccessCopy -> text "is copy"
    ref         = if p_isRef  p then text "is ref"  else empty
    lazy        = if p_isLazy p then text "is lazy" else empty
    -- slots = hsep [text ("is " ++ (cast aux)) <+> text "..." | (aux, val) <- Map.toList $ p_slots p] XXX: for when traits have args
    slots       = hsep [text ("is " ++ (cast $ fst trait)) | trait <- Map.toList $ p_slots p]
    constraints = hsep $ replicate (length $ p_constraints p) (text "where {...}")
    debugDump   = if True then empty else braces $ text $ show p -- XXX delme
--------------------------------------------------------------------------------------

-- | a Capture is a frozen version of the arguments to an application.
data Capt a
    = CaptMeth
        { c_invocant :: a
        , c_feeds    :: [Feed a]
        }
    | CaptSub
        { c_feeds    :: [Feed a]
        }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | non-invocant arguments.
data Feed a = MkFeed
    { f_positionals :: [a]
    , f_nameds      :: Map.Map ID [a]   -- ^ maps to [a] and not a since if the Sig stipulates
                                    --   @x, "x => 1, x => 2" constructs @x = (1, 2).
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

emptyFeed :: Feed a
emptyFeed = MkFeed [] Map.empty

-- | Runtime Capture with dynamic Exp for leaves
--type ExpCapt = Capt Exp
-- | Static Capture with Val for leaves
type ValCapt = Capt Val
type ValFeed = Feed Val

instance ICoercible P ValCapt where
        asStr _ = return (cast "<capt>") -- XXX

