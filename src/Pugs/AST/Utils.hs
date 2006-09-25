{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}

module Pugs.AST.Utils where
import Pugs.Internals
import Pugs.Types
import qualified Data.Set       as Set
import qualified Data.IntMap    as IntMap

import Pugs.AST.SIO
import Pugs.AST.Eval
import {-# SOURCE #-} Pugs.AST.Internals

errIndex :: Show a => Maybe b -> a -> Eval b
errIndex (Just v) _ = return v
errIndex _ idx =
    retError "Modification of non-creatable array value attempted" idx

-- Three outcomes: Has value; can extend; cannot extend
getIndex :: Int -> Maybe a -> Eval [a] -> Maybe (Eval b) -> Eval a

getIndex idx def doList _ | idx < 0 = do
    -- first, check if the list is at least abs(idx) long.
    list <- doList
    if null (drop (abs (idx+1)) list)
        then errIndex def idx
        else return (list !! (idx `mod` (length list)))

-- now we are all positive; either extend or return
getIndex idx def doList ext = do
    list <- doList
    case drop idx list of
        [] -> case ext of
            Just doExt -> do { doExt ; getIndex idx def doList Nothing }
            Nothing    -> errIndex def idx
        (a:_) -> return a

getMapIndex :: Int -> Maybe a -> Eval (IntMap a) -> Maybe (Eval b) -> Eval a
getMapIndex idx def doList _ | idx < 0 = do
    -- first, check if the list is at least abs(idx) long.
    list <- doList
    if IntMap.member (abs (idx+1)) list
        then return . fromJust
            $ IntMap.lookup (idx `mod` (IntMap.size list)) list
        else errIndex def idx
-- now we are all positive; either extend or return
getMapIndex idx def doList ext = do
    list <- doList
    case IntMap.lookup idx list of
        Just a  -> return a
        Nothing -> case ext of
            Just doExt -> do { doExt ; getMapIndex idx def doList Nothing }
            Nothing    -> errIndex def idx

{-|
If we are in list context (i.e. 'CxtSlurpy'), then perform the first
evaluation; otherwise perform the second.
-}
ifListContext :: (MonadReader Env m)
              => m t -- ^ The @then@ case
              -> m t -- ^ The @else@ case
              -> m t
ifListContext trueM falseM = do
    cxt <- asks envContext
    case cxt of
        CxtSlurpy _ -> trueM
        _           -> falseM

errType :: (Typeable a) => a -> String
errType x = show (typeOf x)

createObject :: VType -> [(VStr, Val)] -> Eval VObject
createObject typ attrList = do
    uniq    <- newObjectId
    createObjectRaw uniq Nothing typ attrList

newObjectId :: Eval ObjectId
newObjectId = do
    tv <- asks envMaxId
    liftSTM $ do
        rv <- readTVar tv
        writeTVar tv (MkObjectId (succ (unObjectId rv)))
        return rv
        
castFailM :: forall a b. (Show a, Typeable b) => a -> String -> Eval b
castFailM v str = fail $ "Cannot cast from " ++ show v ++ " to " ++ errType (undefined :: b) ++ " (" ++ str ++ ")"

castFail :: forall a b. (Show a, Typeable b) => a -> String -> b
castFail v str = error $ "Cannot cast from " ++ show v ++ " to " ++ errType (undefined :: b) ++ " (" ++ str ++ ")"

class Unwrap a where
    {-|
    Unwrap a nested expression, throwing away wrappers (such as 'Cxt' or
    'Pos' to get at the more interesting expression underneath. Works both
    on individual 'Exp's, and elementwise on ['Exp']s.
    -}
    unwrap :: a -> a
    unwrap = id

{-|
Represents a junction value.

Note that @VJunc@ is also a pun for a 'Val' constructor /containing/ a 'VJunc'.
-}
data VJunc = MkJunc
    { juncType :: !JuncType -- ^ 'JAny', 'JAll', 'JNone' or 'JOne'
    , juncDup  :: !(Set Val)
    -- ^ Only used for @one()@ junctions. Contains those values
    --     that appear more than once (the actual count is
    --     irrelevant), since matching any of these would
    --     automatically violate the 'match /only/ one value'
    --     junctive semantics.
    , juncSet  :: !(Set Val)
    -- ^ Set of values that make up the junction. In @one()@
    --     junctions, contains the set of values that appear exactly
    --     /once/.
    } deriving (Typeable) {-!derive: YAML_Pos!-}

-- | The combining semantics of a junction. See 'VJunc' for more info.
data JuncType = JAny  -- ^ Matches if /at least one/ member matches
              | JAll  -- ^ Matches only if /all/ members match
              | JNone -- ^ Matches only if /no/ members match
              | JOne  -- ^ Matches if /exactly one/ member matches
    deriving (Eq, Ord, Typeable) {-!derive: YAML_Pos!-}


showRat :: VRat -> VStr
showRat r
    | frac == 0 = s ++ show quot
    | otherwise = s ++ show quot ++ "." ++ showFrac frac
    where
    n = numerator r
    d = denominator r
    s = if signum n < 0 then "-" else ""
    (quot, rem) = quotRem (abs n) d
    frac :: VInt
    frac = round ((rem * (10 ^ (40 :: VInt))) % d)
    showFrac = reverse . dropWhile (== '0') . reverse . pad . show
    pad x = (replicate (40 - length x) '0') ++ x

showTrueRat :: VRat -> VStr
showTrueRat r =
    (show n) ++ "/" ++ (show d)
    where
    n = numerator r
    d = denominator r

showNum :: Show a => a -> String
showNum x
    | str == "Infinity"
    = "Inf"
    | str == "-Infinity"
    = "-Inf"
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x

-- can be factored
{-|
Return the context implied by a particular primary sigil
(\$, \@, \% or \&). E.g. used to find what context to impose on
the RHS of a binding (based on the sigil of the LHS).
-}
cxtOfSigil :: VarSigil -> Cxt
cxtOfSigil SScalar      = cxtItemAny
cxtOfSigil SArray       = cxtSlurpyAny
cxtOfSigil SArrayMulti  = cxtSlurpyAny
cxtOfSigil SHash        = cxtSlurpyAny
cxtOfSigil SCapture     = CxtItem $ mkType "Capture"
cxtOfSigil SCode        = CxtItem $ mkType "Code"
cxtOfSigil SCodeMulti   = CxtItem $ mkType "Code"
cxtOfSigil SRegex       = CxtItem $ mkType "Regex"
cxtOfSigil SType        = CxtItem $ mkType "Type"

cxtOfSigilVar :: Var -> Cxt
cxtOfSigilVar = cxtOfSigil . v_sigil

{-|
Return the type of variable implied by a name beginning with the specified
sigil.
-}
typeOfSigil :: VarSigil -> Type
typeOfSigil SScalar     = mkType "Item"
typeOfSigil SArray      = mkType "Array"
typeOfSigil SArrayMulti = mkType "Array"
typeOfSigil SHash       = mkType "Hash"
typeOfSigil SCode       = mkType "Code"
typeOfSigil SCodeMulti  = mkType "Code"
typeOfSigil SCapture    = mkType "Capture"
typeOfSigil SRegex      = mkType "Regex"
typeOfSigil SType       = mkType "Type"

typeOfSigilVar :: Var -> Type
typeOfSigilVar = typeOfSigil . v_sigil

