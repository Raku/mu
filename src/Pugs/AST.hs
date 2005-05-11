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
    Eval,
    Exp(..),
    Env(..),
    Val(..),
    Value(..),

    Pad(..), Ident, Unwrap(..), Param(..), Params, Bindings, SlurpLimit,
    
    VRef(..), VOpaque(..), VControl(..), VScalar, VList, VPair,
    VSubst, VArray, VHash, VProcess(..), VThunk(..),
    VMatch(..), mkMatchFail, mkMatchOk,
    VCode(..), SubType(..),
    VJunc(..), JuncType(..),
    VObject(..), VType,

    IVar(..),
    IArray, IArraySlice, IHash, IScalar, ICode, IScalarProxy,
    IScalarLazy, IPairHashSlice, IRule, IHandle, IHashEnv(..),
    IScalarCwd(..),

    ArrayClass(..), CodeClass(..), HandleClass(..), HashClass(..),
    ObjectClass(..), PairClass(..), RuleClass(..), ScalarClass(..),
    ThunkClass(..),

    -- MonadEval(..),

    runEvalSTM, runEvalIO, shiftT, resetT, runEvalMain,
    evalExp,
    undef, defined,
    readRef, writeRef, clearRef, dumpRef, forceRef,
    askGlobal, writeVar, readVar,
    findSymRef, findSym,
    ifListContext, ifValTypeIsa, evalValType, fromVal',
    scalarRef, codeRef, arrayRef, hashRef, thunkRef, pairRef,
    newScalar, newArray, newHandle, newObject,
    proxyScalar, constScalar, lazyScalar, lazyUndef, constArray,
    retError, retControl, retEmpty, retIVar, writeIVar,
    fromVals, refType,
    mkPad, lookupPad, padToList, diffPads, unionPads,
    genMultiSym, genSym,
    mkPrim, mkSub,
    cxtOfSigil, typeOfSigil,
    buildParam, defaultArrayParam, defaultHashParam, defaultScalarParam,
    emptyExp,
    isSlurpy, envWant,
    extract,
    strRangeInf, strRange, strInc, charInc,
    doPair, doHash, doArray,

    module Pugs.AST.Pos,
    module Pugs.AST.Scope,
    module Pugs.AST.SIO,
) where
import Pugs.Internals

import Pugs.AST.Internals
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO

-- |Return an infinite (lazy) Haskell list of the given string and its
-- successors. 'strInc' is used to determine what the \'next\' string is.
-- Is used to implement the @...@ infinite-range operator on strings.
strRangeInf :: String -> [String]
strRangeInf s = (s:strRangeInf (strInc s))

-- |Return a range of strings from the first argument to the second, inclusive
-- (as a Haskell list). 'strInc' is used to determine what the \'next\' string 
-- is. Is used to implement the @..@ range operator on strings.
strRange :: String -> String -> [String]
strRange s1 s2
    | s1 == s2              = [s2]
    | length s1 > length s2 = []
    | otherwise             = (s1:strRange (strInc s1) s2)

-- |Find the successor of a string (i.e. the next string \'after\' it).
-- Special rules are used to handle strings ending in an alphanumeric
-- character; otherwise the last character is simply incremented using
-- 'charInc'.
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

-- |Return the code-point-wise successor of a given character.
charInc :: Char -> Char
charInc x   = chr $ 1 + ord x

