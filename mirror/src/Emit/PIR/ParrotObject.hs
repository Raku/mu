{-# OPTIONS_GHC -fglasgow-exts -fth -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}
module Emit.PIR.ParrotObject where

import Data.Char
import Emit.PIR
import Emit.Common
import Language.Haskell.TH.Syntax

type CnName = String
type Ty = String
data Cn = MkCn CnName [Ty]

genClass :: Ty -> [Cn] -> PIR
genClass ty cons = (genTy ty ++ concatMap (genCn ty) cons)

genNS :: Ty -> [Decl] -> Decl
genNS = DeclNS . genLit

genLit :: Ty -> String
genLit = ("PIR::" ++)

genTy :: Ty -> PIR
genTy ty =
    [ genNS ty
        [ DeclSub "__onload" [SubMETHOD] $ map StmtIns
            [ tempPMC <-- "newclass" $ [lit $ genLit ty]
            ]
        ]
    ]

genCn :: Ty -> Cn -> PIR
genCn ty (MkCn con tys) =
    [ genNS con
        [ DeclSub "__onload" [SubMETHOD] $ map StmtIns
            [ tempPMC <-- "subclass" $ [tempPMC, (lit $ genLit ty), fullTy]
            , tempPMC <-- "getclass" $ [fullTy]
            ] ++
            [ StmtIns $ "addattribute" .- [tempPMC, fullNum num]
            | (num, _) <- [1..] `zip` tys
            ]
        , DeclSub "__init" [SubMETHOD] $
            map StmtIns (concatMap fullArg ([1..] `zip` tys))
        ]
    ]
    where
    fullNum :: Int -> Expression
    fullNum num = lit ('$':'.':show num)
    fullTy = lit $ genLit (ty ++ "::" ++ con)
    fullArg (num, attr) = 
        [ InsNew tempPMC (bareType attr)
        , "setattribute" .- [bare "self", fullNum num, tempPMC]
        ]

bareType :: String -> ObjType
bareType t@('G':'H':'C':'.':_) = BareType $ reverse (takeWhile isAlphaNum (reverse t))
bareType t = BareType ("PIR::" ++ t)

genDec :: Dec -> PIR
genDec (DataD _ ty _ cons _) = genClass (show ty) (map conToCn cons)
genDec x = error $ show x

conToCn :: Con -> Cn
conToCn (NormalC con args) = MkCn (show con) (map (argToTy . snd) args)
conToCn x = error $ show x

argToTy :: Type -> Ty
argToTy (ConT x) = show x
argToTy (AppT ListT (ConT x)) = (show x) ++ "List"
argToTy x = error (show x)
    
test :: IO ()
test = do
    q <- runQ decls
    writeFile "pir.pir" (show $ emit $ concatMap genDec q)
    putStrLn "*** File saved as pir.pir"
    return ()

#ifndef HADDOCK
decls :: Q [Dec]
decls = [d|

    data Decl
        = DeclSub       String [SubFlag] [Stmt]
        | DeclNS        String [Decl]
        | DeclInc       FilePath
    data Stmt
        = StmtComment   String
        | StmtLine      FilePath Int
        | StmtPad       [String] [Stmt]
        | StmtIns       Ins

    data Ins
        = InsLocal      RegType String
        | InsNew        LValue ObjType
        | InsBind       LValue Expression
        | InsAssign     LValue Expression
        | InsPrim       LValue String [Expression]
        | InsFun        [Sig] Expression [Expression]
        | InsTailFun    Expression [Expression]
        | InsLabel      String
        | InsComment    String Ins
        | InsExp        Expression

    data Expression
        = ExpLV LValue
        | ExpLit Literal

    data LValue
        = VAR String
        | PMC Int
        | STR Int
        | INT Int
        | NUM Int
        | KEYED LValue Expression
        | NULL

    data Literal
        = LitStr String
        | LitInt Integer
        | LitNum Double

    data SubFlag
        = SubMAIN
        | SubLOAD
        | SubANON
        | SubMETHOD
        | SubMULTI [ObjType]

    data RegType
        = RegInt
        | RegNum
        | RegStr
        | RegPMC

    data ObjType
        = PerlScalar
        | PerlList
        | PerlHash
        | PerlInt
        | PerlPair
        | PerlRef
        | PerlEnv

    data Sig
        = MkSig [ArgFlag] Expression

    data ArgFlag
        = MkArgFlatten 
        | MkArgSlurpyArray
        | MkArgMaybeFlatten
        | MkArgOptional

    |]
#endif
