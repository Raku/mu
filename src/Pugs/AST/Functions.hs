{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances -fparr #-}
module Pugs.AST.Functions where
import Pugs.Internals
import Pugs.Types
import qualified Data.Set       as Set
import qualified Data.Map       as Map

import Pugs.AST.Eval
import Pugs.AST.Utils
import Pugs.AST.SIO
import qualified Pugs.Val as Val
import {-# SOURCE #-} Pugs.AST.Internals
import Pugs.AST.Types


emptyTraitBlocks :: TraitBlocks
emptyTraitBlocks = MkTraitBlocks [] [] [] [] [] [] [] [] [] [] []

isSlurpy :: Param -> Bool
isSlurpy param = isSlurpyCxt $ paramContext param

{-|
An empty Pad with no symbols.
-}

emptyPad :: Pad
emptyPad = MkPad Map.empty

{-# SPECIALISE readPadEntry :: PadEntry -> Eval VRef #-}
{-# SPECIALISE readPadEntry :: PadEntry -> STM VRef #-}
readPadEntry :: MonadSTM m => PadEntry -> m VRef
readPadEntry PEConstant{ pe_proto = v } = return v
readPadEntry x                             = stm (readTVar (pe_store x))

{-# SPECIALISE writePadEntry :: PadEntry -> VRef -> Eval () #-}
{-# SPECIALISE writePadEntry :: PadEntry -> VRef -> STM () #-}
writePadEntry :: MonadSTM m => PadEntry -> VRef -> m ()
writePadEntry x@PEConstant{} _ = die "Cannot rebind constant" x
writePadEntry x                 v = stm (writeTVar (pe_store x) v)

retShift :: Val -> Eval a
-- retShift = shiftT . const . return
retShift = EvalT . return . RException

catchT :: ((Val -> Eval b) -> Eval Val) -> Eval Val
catchT action = tryT (action retShift)

retConstError :: Val -> Eval b
retConstError val = die "Can't modify constant item" val

paramsToSig :: Params -> Val.Sig
paramsToSig params = 
    Val.MkSig
        { Val.s_invocant = Nothing
        , Val.s_requiredPositionalCount =
            length $ filter (\x -> not (isNamed x) && not (isOptional x)) params
        , Val.s_requiredNames =
            Set.fromList $ map (v_name . paramName) $ filter (not . isOptional) params
        , Val.s_positionalList = map paramToValParam $ filter (not . isNamed) params
        , Val.s_namedSet = Map.fromList $ 
            map (\p -> (v_name (paramName p), paramToValParam p)) $ 
                filter isNamed params
        , Val.s_slurpyScalarList = []  -- XXX unimplemented
        , Val.s_slurpyArray   = Nothing  -- XXX ditto
        , Val.s_slurpyHash    = Nothing  -- XXX yep
        , Val.s_slurpyCode    = Nothing  -- XXX all right
        , Val.s_slurpyCapture = Nothing -- this one is okay as it is ;-)
        }   

paramToValParam :: Param -> Val.SigParam
paramToValParam param = ret
    where 
    ret = Val.MkParam 
        { Val.p_variable    = paramName param
        , Val.p_types       = []
        , Val.p_constraints = []
        , Val.p_unpacking   = Nothing
        , Val.p_default     = Val.MkParamDefault Nothing -- XXX Exp incompatibility
        , Val.p_label       = v_name $ paramName param  -- XXX sigility
        , Val.p_slots       = Map.empty
        , Val.p_hasAccess   = case param of
                                  MkOldParam { isLValue = True, isWritable = False } -> Val.AccessRO
                                  MkOldParam { isLValue = True, isWritable = True }  -> Val.AccessRW
                                  MkOldParam { isLValue = False }                    -> Val.AccessCopy
        , Val.p_isRef       = Val.p_hasAccess ret == Val.AccessRW
        , Val.p_isLazy      = isLazy param
        , Val.p_isContext   = False -- XXX - not yet handled
        }

instance ((:>:) String) Params where
    cast = show . paramsToSig


{-|
Construct a 'VCode' representing a built-in primitive operator.

See "Pugs.Prim" for more info.
-}
mkPrim :: VCode
mkPrim = MkCode
    { isMulti        = True
    , subName        = cast "&"
    , subType        = SubPrim
    , subOuterPads   = []
    , subInnerPad    = emptyPad
--  , subLexical     = emptyPad
    , subPackage     = emptyPkg
    , subAssoc       = ANil
    , subParams      = []
    , subBindings    = []
    , subSlurpLimit  = []
    , subReturns     = anyType
    , subBody        = emptyExp
    , subLValue      = False
    , subCont        = Nothing
    , subStarted     = Nothing
    , subTraitBlocks = emptyTraitBlocks
    }

mkCode :: VCode
mkCode = MkCode
    { isMulti        = False
    , subName        = cast "&"
    , subType        = SubBlock
    , subOuterPads   = []
    , subInnerPad    = emptyPad
--  , subLexical     = emptyPad
    , subPackage     = emptyPkg
    , subAssoc       = ANil
    , subParams      = []
    , subBindings    = []
    , subSlurpLimit  = []
    , subReturns     = anyType
    , subBody        = emptyExp
    , subLValue      = False
    , subCont        = Nothing
    , subStarted     = Nothing
    , subTraitBlocks = emptyTraitBlocks
    } 

mkSub :: VCode
mkSub = MkCode
    { isMulti        = False
    , subName        = cast "&"
    , subType        = SubBlock
    , subOuterPads   = []
    , subInnerPad    = emptyPad
--  , subLexical     = emptyPad
    , subPackage     = emptyPkg
    , subAssoc       = ANil
    , subParams      = []
    , subBindings    = []
    , subSlurpLimit  = []
    , subReturns     = anyType
    , subBody        = emptyExp
    , subLValue      = False
    , subCont        = Nothing
    , subStarted     = Nothing
    , subTraitBlocks = emptyTraitBlocks
    }

fromObject :: (Typeable a) => VObject -> a
fromObject obj = case objOpaque obj of
    Nothing     -> castFail obj "VObject without opaque"
    Just dyn    -> case fromDynamic dyn of
        Nothing -> castFail obj "VObject's opaque not valueable"
        Just x  -> x

buildParam :: String -- ^ Type of the parameter
           -> String -- ^ Parameter-sigil (@:@, @!:@, @?@, @!@, etc.)
           -> String -- ^ Name of the parameter (including primary sigil)
           -> Exp    -- ^ Expression for the param's default value
           -> Param
buildParam typ sigil name e = MkOldParam
    { isInvocant    = False
    , isOptional    = '?' `elem` sigil
    , isNamed       = ':' `elem` sigil
    , isLValue      = True
    , isWritable    = (name == "$_")
    , isLazy        = False
    , paramName     = cast name
    , paramContext  = if '*' `elem` sigil
        then CxtSlurpy typ'
        else CxtItem typ'
    , paramDefault  = e
    }
    where
    typ' = if null typ then anyType else mkType typ

mkCompUnit :: String -> Pad -> Exp -> CompUnit
mkCompUnit = MkCompUnit compUnitVersion

{-# NOINLINE compUnitVersion #-}
compUnitVersion :: Int
compUnitVersion = 18

newDebugInfo :: IO DebugInfo
newDebugInfo = fmap Just (io $ newTVarIO Map.empty)


-- | An empty failed match
mkMatchFail :: VMatch
mkMatchFail = MkMatch False 0 0 "" [] Map.empty

-- | Makes a successful match
mkMatchOk :: Int -> Int -> VStr -> VList -> VHash -> VMatch
mkMatchOk   = MkMatch True


{-
    We (may) have to fix the name, as the user can write things like
        &::("infix:<+>")(2, 3)
    which, without fixName, wouldn't work, as all operators are currently
    stored as &infix:+, i.e. without the brackets.
-}

listToPad :: [(Var, PadEntry)] -> Pad
listToPad entries = MkPad (Map.fromList entries)

-- | Look up a symbol in a 'Pad', returning the ref it is bound to.
lookupPad :: Var -- ^ Symbol to look for
          -> Pad -- ^ Pad to look in
          -> Maybe PadEntry -- ^ Might return 'Nothing' if var is not found
lookupPad key (MkPad pad) = Map.lookup key pad

{-# SPECIALISE findSym :: Var -> Pad -> Eval (STM VRef) #-}
{-# SPECIALISE findSym :: Var -> Pad -> Maybe (STM VRef) #-}
findSym :: Monad m => Var -> Pad -> m (STM VRef)
findSym name pad = case lookupPad name pad of
    Just PEConstant{ pe_proto = v }  -> return (return v)
    Just x                              -> return (readTVar (pe_store x))
    _      -> fail $ "Cannot find variable: " ++ show name

findSymRef :: Var -> Pad -> Eval VRef
findSymRef name pad = stm $ join (findSym name pad)

