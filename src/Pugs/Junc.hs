{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Junction logic.

>   Still round the corner there may wait
>   A new road or a secret gate,
>   And though we pass them by today,
>   Tomorrow we may come this way...
-}

module Pugs.Junc where
import Pugs.Internals
import Pugs.AST
import qualified Data.Set as Set

{-|
Construct a @none(...)@ junction from a list of values.

Delegates to 'opJunc'.
-}
opJuncNone :: [Val] -> Val
opJuncNone = opJunc JNone

{-|
Construct an @all(...)@ junction from a list of values.

Delegates to 'opJunc'.
-}
opJuncAll :: [Val] -> Val
opJuncAll = opJunc JAll

{-|
Construct a n@any(...)@ junction from a list of values.

Delegates to 'opJunc'.
-}
opJuncAny :: [Val] -> Val
opJuncAny = opJunc JAny

{-|
Construct a @one(...)@ junction from a list of values.

Does /not/ delegate to 'opJunc'!
-}
opJuncOne :: [Val] -> Val
opJuncOne args = VJunc (MkJunc JOne dups vals)
    where
    vals = Set.fromList [ v | [v] <- groups ]
    dups = Set.fromList [ v | (v:_:_) <- groups ]
    groups = group $ sort args

{-|
Construct a junction of the specified junctive type, containing all the
values in the list.
-}
opJunc :: JuncType -> [Val] -> Val
opJunc t vals = VJunc $ MkJunc t Set.empty (joined `Set.union` Set.fromList vs)
    where
    joined = Set.unions $ map (\(VJunc s) -> juncSet s) js
    (js, vs) = partition sameType vals
    sameType (VJunc (MkJunc t' _ _))  = t == t'
    sameType _                      = False

{-|
Check if the specified value is a 'VJunc' of one of the specified
junctive types. If it is, return it as a 'VJunc'.
-}
juncTypeIs :: Val -- ^ Value to test
           -> [JuncType] -- ^ Types to check against
           -> Maybe VJunc -- ^ Returns 'Nothing' if the test fails
juncTypeIs v ts
    | (VJunc j) <- v
    , juncType j `elem` ts
    = Just j
    | otherwise
    = Nothing

mergeJunc :: JuncType -> [Val] -> [Val] -> VJunc
mergeJunc j ds vs
    = case j of
       JAny -> MkJunc j (Set.fromList ds) (Set.fromList vs)
       JOne -> MkJunc j dups vals
       x    -> internalError $ "mergeJunc pattern failure: " ++ (show x)
    where
    vals = Set.fromList [ v | [v] <- group $ sort vs ]
    dups = Set.fromList (ds ++ [ v | (v:_:_) <- group $ sort (vs ++ ds) ])

-- OK... Now let's implement the hideously clever autothreading algorithm.
-- First pass - thread thru all() and none()
-- Second pass - thread thru any() and one()

juncApply :: ([ApplyArg] -> Eval Val) -> [ApplyArg] -> Eval Val
juncApply f args
    | this@(_, (pivot:_)) <- break isTotalJunc args
    , VJunc (MkJunc j dups vals) <- argValue pivot
    = do
        vals' <- appSet this vals
        return $ VJunc (MkJunc j dups vals')
    | this@(_, (pivot:_)) <- break isPartialJunc args
    , VJunc (MkJunc j dups vals) <- argValue pivot
    = do
        dups' <- appList this dups
        vals' <- appList this vals
        return $ VJunc (mergeJunc j dups' vals')
    | (val:_) <- [ val | (ApplyArg _ val@(VError _ _) _) <- args ]
    = return val
    | otherwise
    = f args
    where
    appSet :: ([ApplyArg], [ApplyArg]) -> Set Val -> Eval (Set Val)
    appSet x y = return . Set.fromList =<< appList x y
    appList :: ([ApplyArg], [ApplyArg]) -> Set Val -> Eval [Val]
    appList (before, (ApplyArg name _ coll):after) vs = do
        mapM (\v -> juncApply f (before ++ ((ApplyArg name v coll):after))) $ Set.elems vs
    appList _ _ = internalError "appList: list doesn't begin with ApplyArg"

isTotalJunc :: ApplyArg -> Bool
isTotalJunc arg
    | (ApplyArg _ (VJunc j) b) <- arg
    , (juncType j ==) `any` [JAll, JNone]
    = not b
    | otherwise
    = False

isPartialJunc :: ApplyArg -> Bool
isPartialJunc arg
    | (ApplyArg _ (VJunc j) b) <- arg
    , (juncType j ==) `any` [JOne, JAny]
    = not b
    | otherwise
    = False

data ApplyArg = ApplyArg
    { argName       :: String
    , argValue      :: Val
    , argCollapsed  :: Bool
    }
    deriving (Show, Eq, Ord)

---------------------

{-

bool = ["0", "1"]
test = do
    sequence [ testJunc out inn nest | out <- junc, inn <- junc, nest <- [True, False] ]

testJunc out inn nest = do
    let foo = [ blah a b c out inn nest | a <- bool, b <- bool, c <- bool ]
    when (all id foo) $ if nest
        then print (out, inn)
        else print (out)

junc = ["any", "one", "all", "none"]
out1 = "any"
inn1 = "any"

blah a b c out inn nest = want == has
    where
    want = opEval emptyEnv $ "? " ++ out1 ++ "( " ++ inn1 ++ "( " ++ a ++ ", " ++ b ++ " ), " ++ inn1 ++ "( " ++ b ++ ", " ++ " " ++ c ++ " ) )"
    has | nest = opEval emptyEnv $ "? " ++ out ++ "( " ++ b ++ ", " ++ inn ++ "(" ++ a ++ ", " ++ c ++ "))"
        | otherwise = opEval emptyEnv $ "? " ++ out ++ "( " ++ b ++ ", " ++ a ++ ", " ++ c ++ ")"
-}

