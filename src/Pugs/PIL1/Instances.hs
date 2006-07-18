{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fno-warn-orphans -fno-warn-incomplete-patterns -fallow-undecidable-instances -cpp #-}




{- 
-- WARNING WARNING WARNING --

This is an autogenerated file from src/Pugs/PIL1.hs.

Do not edit this file.

All changes made here will be lost!

-- WARNING WARNING WARNING --
-}

#ifndef HADDOCK









module Pugs.PIL1.Instances ()
where
import Pugs.PIL1
import Data.Yaml.Syck
import DrIFT.YAML
import DrIFT.JSON
import DrIFT.Perl5
import Control.Monad
import qualified Data.ByteString as Buf

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Perl5 PIL_Environment where
    showPerl5 (PIL_Environment aa ab) =
	      showP5HashObj "PIL::Environment"
	      [("pilGlob", showPerl5 aa) , ("pilMain", showPerl5 ab)]

instance JSON PIL_Environment where
    showJSON (PIL_Environment aa ab) = showJSHashObj "PIL_Environment"
	     [("pilGlob", showJSON aa), ("pilMain", showJSON ab)]

instance YAML PIL_Environment where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PIL_Environment" -> do
	    let EMap assocs = e
	    let [aa, ab] = map snd assocs
	    liftM2 PIL_Environment (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PIL_Environment"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PIL_Environment aa ab) = asYAMLmap "PIL_Environment"
	   [("pilGlob", asYAML aa), ("pilMain", asYAML ab)]

instance Perl5 PIL_Stmts where
    showPerl5 (PNil) = showP5Class "PNil"
    showPerl5 (PStmts aa ab) = showP5HashObj "PStmts"
	      [("pStmt", showPerl5 aa) , ("pStmts", showPerl5 ab)]
    showPerl5 (PPad aa ab ac) = showP5HashObj "PPad"
	      [("pScope", showPerl5 aa) , ("pSyms", showPerl5 ab) ,
	       ("pStmts", showPerl5 ac)]

instance JSON PIL_Stmts where
    showJSON (PNil) = showJSScalar "PNil"
    showJSON (PStmts aa ab) = showJSHashObj "PStmts"
	     [("pStmt", showJSON aa), ("pStmts", showJSON ab)]
    showJSON (PPad aa ab ac) = showJSHashObj "PPad"
	     [("pScope", showJSON aa), ("pSyms", showJSON ab),
	      ("pStmts", showJSON ac)]

instance YAML PIL_Stmts where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PNil" -> do
	    return PNil
	"PStmts" -> do
	    let EMap assocs = e
	    let [aa, ab] = map snd assocs
	    liftM2 PStmts (fromYAML aa) (fromYAML ab)
	"PPad" -> do
	    let EMap assocs = e
	    let [aa, ab, ac] = map snd assocs
	    liftM3 PPad (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PNil","PStmts","PPad"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PNil) = asYAMLcls "PNil"
    asYAML (PStmts aa ab) = asYAMLmap "PStmts"
	   [("pStmt", asYAML aa), ("pStmts", asYAML ab)]
    asYAML (PPad aa ab ac) = asYAMLmap "PPad"
	   [("pScope", asYAML aa), ("pSyms", asYAML ab),
	    ("pStmts", asYAML ac)]

instance Perl5 PIL_Stmt where
    showPerl5 (PNoop) = showP5Class "PNoop"
    showPerl5 (PStmt aa) = showP5HashObj "PStmt"
	      [("pExpr", showPerl5 aa)]
    showPerl5 (PPos aa ab ac) = showP5HashObj "PPos"
	      [("pPos", showPerl5 aa) , ("pExp", showPerl5 ab) ,
	       ("pNode", showPerl5 ac)]

instance JSON PIL_Stmt where
    showJSON (PNoop) = showJSScalar "PNoop"
    showJSON (PStmt aa) = showJSHashObj "PStmt"
	     [("pExpr", showJSON aa)]
    showJSON (PPos aa ab ac) = showJSHashObj "PPos"
	     [("pPos", showJSON aa), ("pExp", showJSON ab),
	      ("pNode", showJSON ac)]

instance YAML PIL_Stmt where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PNoop" -> do
	    return PNoop
	"PStmt" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PStmt (fromYAML aa)
	"PPos" -> do
	    let EMap assocs = e
	    let [aa, ab, ac] = map snd assocs
	    liftM3 PPos (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PNoop","PStmt","PPos"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PNoop) = asYAMLcls "PNoop"
    asYAML (PStmt aa) = asYAMLmap "PStmt" [("pExpr", asYAML aa)]
    asYAML (PPos aa ab ac) = asYAMLmap "PPos"
	   [("pPos", asYAML aa), ("pExp", asYAML ab), ("pNode", asYAML ac)]

instance Perl5 PIL_Expr where
    showPerl5 (PRawName aa) = showP5HashObj "PRawName"
	      [("pRawName", showPerl5 aa)]
    showPerl5 (PExp aa) = showP5HashObj "PExp" [("pLV", showPerl5 aa)]
    showPerl5 (PLit aa) = showP5HashObj "PLit" [("pLit", showPerl5 aa)]
    showPerl5 (PThunk aa) = showP5HashObj "PThunk"
	      [("pThunk", showPerl5 aa)]
    showPerl5 (PCode aa ab ac ad ae) = showP5HashObj "PCode"
	      [("pType", showPerl5 aa) , ("pParams", showPerl5 ab) ,
	       ("pLValue", showPerl5 ac) , ("pIsMulti", showPerl5 ad) ,
	       ("pBody", showPerl5 ae)]

instance JSON PIL_Expr where
    showJSON (PRawName aa) = showJSHashObj "PRawName"
	     [("pRawName", showJSON aa)]
    showJSON (PExp aa) = showJSHashObj "PExp" [("pLV", showJSON aa)]
    showJSON (PLit aa) = showJSHashObj "PLit" [("pLit", showJSON aa)]
    showJSON (PThunk aa) = showJSHashObj "PThunk"
	     [("pThunk", showJSON aa)]
    showJSON (PCode aa ab ac ad ae) = showJSHashObj "PCode"
	     [("pType", showJSON aa), ("pParams", showJSON ab),
	      ("pLValue", showJSON ac), ("pIsMulti", showJSON ad),
	      ("pBody", showJSON ae)]

instance YAML PIL_Expr where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PRawName" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PRawName (fromYAML aa)
	"PExp" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PExp (fromYAML aa)
	"PLit" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PLit (fromYAML aa)
	"PThunk" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PThunk (fromYAML aa)
	"PCode" -> do
	    let EMap assocs = e
	    let [aa, ab, ac, ad, ae] = map snd assocs
	    liftM5 PCode (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PRawName","PExp","PLit","PThunk","PCode"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PRawName aa) = asYAMLmap "PRawName"
	   [("pRawName", asYAML aa)]
    asYAML (PExp aa) = asYAMLmap "PExp" [("pLV", asYAML aa)]
    asYAML (PLit aa) = asYAMLmap "PLit" [("pLit", asYAML aa)]
    asYAML (PThunk aa) = asYAMLmap "PThunk" [("pThunk", asYAML aa)]
    asYAML (PCode aa ab ac ad ae) = asYAMLmap "PCode"
	   [("pType", asYAML aa), ("pParams", asYAML ab),
	    ("pLValue", asYAML ac), ("pIsMulti", asYAML ad),
	    ("pBody", asYAML ae)]

instance Perl5 PIL_Decl where
    showPerl5 (PSub aa ab ac ad ae af) = showP5HashObj "PSub"
	      [("pSubName", showPerl5 aa) , ("pSubType", showPerl5 ab) ,
	       ("pSubParams", showPerl5 ac) , ("pSubLValue", showPerl5 ad) ,
	       ("pSubIsMulti", showPerl5 ae) , ("pSubBody", showPerl5 af)]

instance JSON PIL_Decl where
    showJSON (PSub aa ab ac ad ae af) = showJSHashObj "PSub"
	     [("pSubName", showJSON aa), ("pSubType", showJSON ab),
	      ("pSubParams", showJSON ac), ("pSubLValue", showJSON ad),
	      ("pSubIsMulti", showJSON ae), ("pSubBody", showJSON af)]

instance YAML PIL_Decl where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PSub" -> do
	    let liftM6 f m1 m2 m3 m4 m5 m6 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6)}
	    let EMap assocs = e
	    let [aa, ab, ac, ad, ae, af] = map snd assocs
	    liftM6 PSub (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PSub"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PSub aa ab ac ad ae af) = asYAMLmap "PSub"
	   [("pSubName", asYAML aa), ("pSubType", asYAML ab),
	    ("pSubParams", asYAML ac), ("pSubLValue", asYAML ad),
	    ("pSubIsMulti", asYAML ae), ("pSubBody", asYAML af)]

instance Perl5 PIL_Literal where
    showPerl5 (PVal aa) = showP5HashObj "PVal" [("pVal", showPerl5 aa)]

instance JSON PIL_Literal where
    showJSON (PVal aa) = showJSHashObj "PVal" [("pVal", showJSON aa)]

instance YAML PIL_Literal where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PVal" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PVal (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PVal"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PVal aa) = asYAMLmap "PVal" [("pVal", asYAML aa)]

instance Perl5 PIL_LValue where
    showPerl5 (PVar aa) = showP5HashObj "PVar"
	      [("pVarName", showPerl5 aa)]
    showPerl5 (PApp aa ab ac ad) = showP5HashObj "PApp"
	      [("pCxt", showPerl5 aa) , ("pFun", showPerl5 ab) ,
	       ("pInv", showPerl5 ac) , ("pArgs", showPerl5 ad)]
    showPerl5 (PAssign aa ab) = showP5HashObj "PAssign"
	      [("pLHS", showPerl5 aa) , ("pRHS", showPerl5 ab)]
    showPerl5 (PBind aa ab) = showP5HashObj "PBind"
	      [("pLHS", showPerl5 aa) , ("pRHS", showPerl5 ab)]

instance JSON PIL_LValue where
    showJSON (PVar aa) = showJSHashObj "PVar"
	     [("pVarName", showJSON aa)]
    showJSON (PApp aa ab ac ad) = showJSHashObj "PApp"
	     [("pCxt", showJSON aa), ("pFun", showJSON ab),
	      ("pInv", showJSON ac), ("pArgs", showJSON ad)]
    showJSON (PAssign aa ab) = showJSHashObj "PAssign"
	     [("pLHS", showJSON aa), ("pRHS", showJSON ab)]
    showJSON (PBind aa ab) = showJSHashObj "PBind"
	     [("pLHS", showJSON aa), ("pRHS", showJSON ab)]

instance YAML PIL_LValue where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"PVar" -> do
	    let EMap assocs = e
	    let [aa] = map snd assocs
	    liftM PVar (fromYAML aa)
	"PApp" -> do
	    let EMap assocs = e
	    let [aa, ab, ac, ad] = map snd assocs
	    liftM4 PApp (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad)
	"PAssign" -> do
	    let EMap assocs = e
	    let [aa, ab] = map snd assocs
	    liftM2 PAssign (fromYAML aa) (fromYAML ab)
	"PBind" -> do
	    let EMap assocs = e
	    let [aa, ab] = map snd assocs
	    liftM2 PBind (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["PVar","PApp","PAssign","PBind"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (PVar aa) = asYAMLmap "PVar" [("pVarName", asYAML aa)]
    asYAML (PApp aa ab ac ad) = asYAMLmap "PApp"
	   [("pCxt", asYAML aa), ("pFun", asYAML ab), ("pInv", asYAML ac),
	    ("pArgs", asYAML ad)]
    asYAML (PAssign aa ab) = asYAMLmap "PAssign"
	   [("pLHS", asYAML aa), ("pRHS", asYAML ab)]
    asYAML (PBind aa ab) = asYAMLmap "PBind"
	   [("pLHS", asYAML aa), ("pRHS", asYAML ab)]

instance Perl5 TParam where
    showPerl5 (MkTParam aa ab) = showP5HashObj "MkTParam"
	      [("tpParam", showPerl5 aa) , ("tpDefault", showPerl5 ab)]

instance JSON TParam where
    showJSON (MkTParam aa ab) = showJSHashObj "MkTParam"
	     [("tpParam", showJSON aa), ("tpDefault", showJSON ab)]

instance YAML TParam where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkTParam" -> do
	    let EMap assocs = e
	    let [aa, ab] = map snd assocs
	    liftM2 MkTParam (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkTParam"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkTParam aa ab) = asYAMLmap "MkTParam"
	   [("tpParam", asYAML aa), ("tpDefault", asYAML ab)]

instance Perl5 TCxt where
    showPerl5 (TCxtVoid) = showP5Class "TCxtVoid"
    showPerl5 (TCxtLValue aa) = showP5ArrayObj "TCxtLValue"
	      [showPerl5 aa]
    showPerl5 (TCxtItem aa) = showP5ArrayObj "TCxtItem" [showPerl5 aa]
    showPerl5 (TCxtSlurpy aa) = showP5ArrayObj "TCxtSlurpy"
	      [showPerl5 aa]
    showPerl5 (TTailCall aa) = showP5ArrayObj "TTailCall"
	      [showPerl5 aa]

instance JSON TCxt where
    showJSON (TCxtVoid) = showJSScalar "TCxtVoid"
    showJSON (TCxtLValue aa) = showJSArrayObj "TCxtLValue"
	     [showJSON aa]
    showJSON (TCxtItem aa) = showJSArrayObj "TCxtItem" [showJSON aa]
    showJSON (TCxtSlurpy aa) = showJSArrayObj "TCxtSlurpy"
	     [showJSON aa]
    showJSON (TTailCall aa) = showJSArrayObj "TTailCall" [showJSON aa]

instance YAML TCxt where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"TCxtVoid" -> do
	    return TCxtVoid
	"TCxtLValue" -> do
	    let ESeq [aa] = e
	    liftM TCxtLValue (fromYAML aa)
	"TCxtItem" -> do
	    let ESeq [aa] = e
	    liftM TCxtItem (fromYAML aa)
	"TCxtSlurpy" -> do
	    let ESeq [aa] = e
	    liftM TCxtSlurpy (fromYAML aa)
	"TTailCall" -> do
	    let ESeq [aa] = e
	    liftM TTailCall (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["TCxtVoid","TCxtLValue","TCxtItem","TCxtSlurpy","TTailCall"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (TCxtVoid) = asYAMLcls "TCxtVoid"
    asYAML (TCxtLValue aa) = asYAMLseq "TCxtLValue" [asYAML aa]
    asYAML (TCxtItem aa) = asYAMLseq "TCxtItem" [asYAML aa]
    asYAML (TCxtSlurpy aa) = asYAMLseq "TCxtSlurpy" [asYAML aa]
    asYAML (TTailCall aa) = asYAMLseq "TTailCall" [asYAML aa]

instance Perl5 TEnv where
    showPerl5 (MkTEnv aa ab ac ad ae) = showP5HashObj "MkTEnv"
	      [("tLexDepth", showPerl5 aa) , ("tTokDepth", showPerl5 ab) ,
	       ("tCxt", showPerl5 ac) , ("tReg", showPerl5 ad) ,
	       ("tLabel", showPerl5 ae)]

instance JSON TEnv where
    showJSON (MkTEnv aa ab ac ad ae) = showJSHashObj "MkTEnv"
	     [("tLexDepth", showJSON aa), ("tTokDepth", showJSON ab),
	      ("tCxt", showJSON ac), ("tReg", showJSON ad),
	      ("tLabel", showJSON ae)]

instance YAML TEnv where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkTEnv" -> do
	    let EMap assocs = e
	    let [aa, ab, ac, ad, ae] = map snd assocs
	    liftM5 MkTEnv (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkTEnv"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkTEnv aa ab ac ad ae) = asYAMLmap "MkTEnv"
	   [("tLexDepth", asYAML aa), ("tTokDepth", asYAML ab),
	    ("tCxt", asYAML ac), ("tReg", asYAML ad), ("tLabel", asYAML ae)]

--  Imported from other files :-

type Buf = Buf.ByteString

#endif
