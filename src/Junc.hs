{-# OPTIONS -fglasgow-exts #-}

{-
    Junction logic.

    Still round the corner there may wait
    A new road or a secret gate,
    And though we pass them by today,
    Tomorrow we may come this way...
-}

module Junc where
import Internals
import AST

opJuncAll = opJunc JAll
opJuncAny = opJunc JAny
opJuncOne args = VJunc (Junc JOne dups vals)
    where
    vals = mkSet [ v | [v] <- groups ]
    dups = mkSet [ v | (v:_:_) <- groups ]
    groups = group $ sort args

opJunc :: JuncType -> [Val] -> Val
opJunc t vals = VJunc $ Junc t emptySet (joined `union` mkSet vs)
    where
    joined = unionManySets $ map (\(VJunc s) -> juncSet s) js
    (js, vs) = partition sameType vals
    sameType (VJunc (Junc t' _ _))  = t == t'
    sameType _                      = False

juncTypeIs :: Val -> [JuncType] -> Maybe VJunc
juncTypeIs v ts
    | (VJunc j) <- v
    , juncType j `elem` ts
    = Just j
    | otherwise
    = Nothing

mergeJunc j ds vs
    = case j of
       JAny -> Junc j (mkSet ds) (mkSet vs)
       JOne -> Junc j dups vals
       x    -> internalError $ "mergeJunk pattern failure: " ++ (show x)
    where
    vals = mkSet [ v | [v] <- group $ sort vs ]
    dups = mkSet (ds ++ [ v | (v:_:_) <- group $ sort (vs ++ ds) ])

juncApply :: ([ApplyArg] -> Eval Val) -> [ApplyArg] -> Eval Val
juncApply f args
    | this@(_, (pivot:_)) <- break isTotalJunc args
    , VJunc (Junc j dups vals) <- argValue pivot
    = do
        vals' <- appSet this vals
        return $ VJunc (Junc j dups vals')
    | this@(_, (pivot:_)) <- break isPartialJunc args
    , VJunc (Junc j dups vals) <- argValue pivot
    = do
        dups' <- appList this dups
        vals' <- appList this vals
        return $ VJunc (mergeJunc j dups' vals')
    | (val:_) <- [ val | (ApplyArg _ val@(VError _ _) _) <- args ]
    = return val
    | otherwise
    = f args
    where
    appSet x y = return . mkSet =<< appList x y
    appList (before, (ApplyArg name _ coll):after) vs = do
        mapM (\v -> juncApply f (before ++ ((ApplyArg name v coll):after))) $ setToList vs
    appList _ _ = internalError "appList: list doesn't begin with ApplyArg"

isTotalJunc arg
    | (ApplyArg _ (VJunc j) b) <- arg
    , (juncType j ==) `any` [JAll, JNone]
    = not b
    | otherwise
    = False

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
