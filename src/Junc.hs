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
opJuncOne vals
    | length (nub vals) == length vals
    = VJunc JOne $ mkSet vals
    | otherwise
    = VJunc JOne emptySet

opJunc :: JuncType -> [Val] -> Val
opJunc j vals = VJunc j $ joined `union` mkSet vs
    where
    joined = unionManySets $ map juncValues js
    (js, vs) = partition sameType vals
    sameType (VJunc j' _)   = (j == j')
    sameType _              = False

juncValues :: Val -> VJunc
juncValues (VJunc _ l)  = l
juncValues _            = emptySet

juncType :: Val -> Maybe (JuncType, VJunc)
juncType v
    | VJunc j l <- v
    = Just (j, l)
    | otherwise
    = Nothing

juncTypeIs :: Val -> [JuncType] -> Maybe (JuncType, VJunc)
juncTypeIs v js
    | Just (j, l) <- juncType v
    , j `elem` js
    = Just (j, l)
    | otherwise
    = Nothing


juncApply f args
    | (before, (ApplyArg name (VJunc j vs) coll):after) <- break isTotalJunc args
    = VJunc j $ mapSet (\v -> juncApply f (before ++ ((ApplyArg name v coll):after))) vs
    | (before, (ApplyArg name (VJunc j vs) coll):after) <- break isPartialJunc args
    = VJunc j $ mapSet (\v -> juncApply f (before ++ ((ApplyArg name v coll):after))) vs
    | (val:_) <- [ val | (ApplyArg _ val@(VError _ _) _) <- args ]
    = val
    | otherwise
    = f args

isTotalJunc (ApplyArg _ (VJunc JAll _) b)   = not b
isTotalJunc (ApplyArg _ (VJunc JNone _) b)  = not b
isTotalJunc _                   = False

isPartialJunc (ApplyArg _ (VJunc JOne _) b) = not b
isPartialJunc (ApplyArg _ (VJunc JAny _) b) = not b
isPartialJunc _                 = False


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
