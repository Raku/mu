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

    IVar(..),
    IArray, IArraySlice, IHash, IScalar, ICode, IScalarProxy,
    IScalarLazy, IPairHashSlice, IRule, IHandle, IHashEnv(..),
    IScalarCwd(..),

    ArrayClass(..), CodeClass(..), HandleClass(..), HashClass(..),
    ObjectClass(..), PairClass(..), RuleClass(..), ScalarClass(..),
    ThunkClass(..),

    MonadSTM(..),
    -- MonadEval(..),

    runSTM, runIO, runEvalSTM, runEvalIO, shiftT, resetT, runEvalMain,
    evalExp,
    undef, defined,
    readRef, writeRef, clearRef, dumpRef, forceRef,
    askGlobal, writeVar, readVar,
    findSymRef, findSym,
    ifListContext, ifValTypeIsa, evalValType, fromVal',
    scalarRef, codeRef, arrayRef, hashRef, thunkRef, pairRef,
    newScalar, newArray, newHandle, newObject,
    proxyScalar, constScalar, lazyScalar, lazyUndef, constArray,
    retError, retControl, retEmpty, retIVar,
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

    -- TODO: move to Pugs.Parser.NaturalOrRat
    naturalOrRat,

    module Pugs.AST.Pos,
    module Pugs.AST.Scope,
) where

import Pugs.AST.Internals
import Pugs.AST.Pos
import Pugs.AST.Scope
