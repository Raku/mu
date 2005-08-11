{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Abstract syntax tree.

>   Tall ships and tall kings
>   Three times three.
>   What brought they from the foundered land
>   Over the flowing sea?
>   Seven stars and seven stones
>   And one white tree.
-}

module Pugs.AST (
    evalExp,
    genMultiSym, genSym,
    strRangeInf, strRange, strInc, charInc,
    mergeStmts, isEmptyParams,
    newPackage,

    module Pugs.AST.Internals,
    module Pugs.AST.Pos,
    module Pugs.AST.Scope,
    module Pugs.AST.SIO,
    module Pugs.AST.Pad,
) where
import Pugs.Internals
import Pugs.Types
import qualified Data.Map as Map

import Pugs.AST.Internals
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.AST.Pad

{-|
Return an infinite (lazy) Haskell list of the given string and its
successors. 'strInc' is used to determine what the \'next\' string is.

Used to implement the @...@ infinite-range operator on strings.
-}
strRangeInf :: String -> [String]
strRangeInf s = (s:strRangeInf (strInc s))

{-|
Return a range of strings from the first argument to the second, inclusive
(as a Haskell list). 'strInc' is used to determine what the \'next\' string 
is.

Used to implement the @..@ range operator on strings.
-}
strRange :: String -> String -> [String]
strRange s1 s2
    | s1 == s2              = [s2]
    | length s1 > length s2 = []
    | otherwise             = (s1:strRange (strInc s1) s2)

{-|
Find the successor of a string (i.e. the next string \'after\' it).
Special rules are used to handle wraparound for strings ending in an
alphanumeric character; otherwise the last character is simply incremented 
using 'charInc'.
-}
strInc :: String -> String
strInc []       = "1"
strInc "z"      = "aa"
strInc "Z"      = "AA"
strInc "9"      = "10"
strInc str
    | x == 'z'  = strInc xs ++ "a"
    | x == 'Z'  = strInc xs ++ "A"
    | x == '9'  = strInc xs ++ "0"
    | otherwise = xs ++ [charInc x]
    where
    x   = last str
    xs  = init str

-- | Return the code-point-wise successor of a given character.
charInc :: Char -> Char
charInc x   = chr $ 1 + ord x

{-|
Evaluate the given expression, using the currently active evaluator
(as given by the 'envEval' slot of the current 'Env').
-}
evalExp :: Exp -> Eval Val
evalExp exp = do
    evl <- asks envEval
    evl exp

{-|
Create a 'Pad'-transforming transaction that will install a symbol
definition in the 'Pad' it is applied to, /alongside/ any other mappings
of the same name. This is to allow for overloaded (i.e. multi) subs,
where one sub name actually maps to /all/ the different multi subs.
(Is this correct?)
-}
genMultiSym :: MonadSTM m => String -> VRef -> m (Pad -> Pad)
genMultiSym name ref = do
    tvar    <- liftSTM $ newTVar ref
    fresh   <- liftSTM $ newTVar True
    return $ \(MkPad map) -> MkPad $
        Map.insertWith (++) name [(fresh, tvar)] map

{-|
Create a 'Pad'-transforming transaction that will install a symbol
mapping from a name to a thing, in the 'Pad' it is applied to.
Unlike 'genMultiSym', this version just installs a single definition
(right?), shadowing any earlier or outer definition.
-}
genSym :: MonadSTM m => String -> VRef -> m (Pad -> Pad)
genSym name ref = do
    tvar    <- liftSTM $ newTVar ref
    fresh   <- liftSTM $ newTVar True
    return $ \(MkPad map) -> MkPad $ Map.insert name [(fresh, tvar)] map

-- Stmt is essentially a cons cell
-- Stmt (Stmt ...) is illegal
mergeStmts :: Exp -> Exp -> Exp
mergeStmts (Stmts x1 x2) y = mergeStmts x1 (mergeStmts x2 y)
mergeStmts Noop y@(Stmts _ _) = y
mergeStmts (Sym scope name x) y = Sym scope name (mergeStmts x y)
mergeStmts (Pad scope lex x) y = Pad scope lex (mergeStmts x y)
mergeStmts (Syn "package" [pkg@(Val (VStr _))]) y =
    Syn "namespace" [pkg, y]
mergeStmts x@(Pos pos (Syn syn _)) y | (syn ==) `any` words "subst match //"  =
    mergeStmts (Pos pos (App (Var "&infix:~~") Nothing [Var "$_", x])) y
mergeStmts x y@(Pos pos (Syn syn _)) | (syn ==) `any` words "subst match //"  =
    mergeStmts x (Pos pos (App (Var "&infix:~~") Nothing [Var "$_", y]))
mergeStmts (Pos pos (Syn "sub" [Val (VCode sub)])) y
    | subType sub >= SubBlock, isEmptyParams (subParams sub) =
    -- bare Block in statement level; annul all its parameters and run it!
    mergeStmts (Pos pos $ App (Val $ VCode sub{ subParams = [] }) Nothing []) y
mergeStmts x (Pos pos (Syn "sub" [Val (VCode sub)]))
    | subType sub >= SubBlock, isEmptyParams (subParams sub) =
    -- bare Block in statement level; annul all its parameters and run it!
    mergeStmts x (Pos pos $ App (Val $ VCode sub{ subParams = [] }) Nothing [])
mergeStmts x (Stmts y Noop) = mergeStmts x y
mergeStmts x (Stmts Noop y) = mergeStmts x y
mergeStmts x y = Stmts x y

isEmptyParams :: [Param] -> Bool
isEmptyParams [] = True
isEmptyParams [x] | [_, '_'] <- paramName x = True
isEmptyParams _ = False

newPackage :: String -> String -> [String] -> Exp
newPackage cls name traits = Sym SGlobal (':':'*':name) $ Syn ":="
    [ Var (':':'*':name)
    , App (Var "&Object::new")
        (Just $ Val (VType $ mkType cls))
        [ App (Var "&infix:=>") Nothing
            [ Val (VStr "traits")
            , Val (VList $ map VStr traits)
            ]
        , App (Var "&infix:=>") Nothing
            [ Val (VStr "name")
            , Val (VStr name)
            ]
        ]
    ]
