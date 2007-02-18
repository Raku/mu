{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances -fparr -foverloaded-strings #-}




{- 
-- WARNING WARNING WARNING --

This is an autogenerated file from src/Pugs/AST/Internals.hs.

Do not edit this file.

All changes made here will be lost!

-- WARNING WARNING WARNING --
-}

#ifndef HADDOCK









module Pugs.AST.Internals.Instances ()
where
import Pugs.AST.Internals
import Data.Yaml.Syck
import DrIFT.YAML
import DrIFT.JSON
import DrIFT.Perl5
import DrIFT.Perl6Class
import Control.Monad
import qualified Data.ByteString as Buf

import Pugs.AST.Scope
import Pugs.AST.Pos
import Pugs.AST.Prag
import Pugs.AST.SIO
import Pugs.Types
import Pugs.Internals
import Pugs.Embed.Perl5
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import qualified Pugs.Val       as Val

import qualified Data.HashTable    as H

{-# NOINLINE _FakeEnv #-}
_FakeEnv :: Env
_FakeEnv = unsafePerformIO $ liftSTM $ do
    ref  <- newTVar Map.empty
    glob <- newTVar $ MkPad Map.empty
    init <- newTVar $ MkInitDat { initPragmas=[] }
    maxi <- newTVar $ MkObjectId 1
    return $ MkEnv
        { envContext = CxtVoid
        , envLexical = MkPad Map.empty
        , envImplicit= Map.empty
        , envLValue  = False
        , envGlobal  = glob
        , envPackage = _cast "Main"
        , envClasses = initTree
        , envEval    = const (return VUndef)
        , envCaller  = Nothing
        , envOuter   = Nothing
        , envFrames  = Set.empty
        , envBody    = Val undef
        , envDebug   = Just ref -- Set to "Nothing" to disable debugging
        , envPos     = MkPos "<null>" 1 1 1 1
        , envPragmas = []
        , envInitDat = init
        , envMaxId   = maxi
        , envAtomic  = False
        }

fakeEval :: MonadIO m => Eval Val -> m Val
fakeEval = liftIO . runEvalIO _FakeEnv

instance YAML Val.Val
instance YAML ([Val] -> Eval Val) where
    asYAML _ = return nilNode
    fromYAML _ = return (const $ return VUndef)
instance YAML (Maybe Env) where
    asYAML _ = return nilNode
    fromYAML _ = return Nothing
instance YAML (Eval Val) where
    asYAML x = asYAML =<< fakeEval x
    fromYAML x = return =<< fromYAML x
instance YAML a => YAML (Map String a) where
    asYAML x = asYAMLmap "Map" $ Map.toAscList (Map.map asYAML x)
    fromYAML node = fmap Map.fromList (fromYAMLmap node)
instance YAML a => YAML (Map ByteString a) where
    asYAML x = asYAMLmapBuf "Map" $ Map.toAscList (Map.map asYAML x)
    fromYAML node = fmap Map.fromList (fromYAMLmapBuf node)
instance YAML a => YAML (Map Var a) where
    asYAML x = asYAMLmap "Map" . sortBy (\x y -> fst x `compare` fst y) $
        [ (cast k, asYAML v) | (k, v) <- Map.toList x ]
    fromYAML node = do
        list <- fromYAMLmapBuf node
        return (Map.fromList [ (cast k, v) | (k, v) <- list ])
instance Typeable a => YAML (IVar a) where
    asYAML x = asYAML (MkRef x)
instance YAML VRef where
    asYAML (MkRef (ICode cv)) = do
        VCode vsub  <- fakeEval $ fmap VCode (code_fetch cv)
        vsubC       <- asYAML vsub
        return $ mkTagNode (tagHs "VCode") $ ESeq [vsubC]
    asYAML (MkRef (IScalar sv)) = do
        val <- fakeEval $ scalar_fetch sv
        svC <- asYAML val
        let tag = if scalar_iType sv == mkType "Scalar::Const"
                    then "VScalar" else "IScalar"
        return $ mkTagNode (tagHs tag) $ ESeq [svC]
    asYAML (MkRef (IArray av)) = do
        VList vals <- fakeEval $ fmap VList (array_fetch av)
        avC <- asYAML vals
        return $ mkTagNode (tagHs "Array") $ ESeq [avC]
    asYAML (MkRef (IHash hv)) = do
        VMatch MkMatch{ matchSubNamed = hv } <- fakeEval $ fmap (VMatch . MkMatch False 0 0 "" []) (hash_fetch hv)
        hvC <- asYAML hv
        return $ mkTagNode (tagHs "Hash") $ ESeq [hvC]
    asYAML (MkRef (IPair pv)) = do
        VList [k, v] <- fakeEval $ fmap (\(k, v) -> VList [k, v]) (pair_fetch pv)
        avC <- asYAML (k, v)
        return $ mkTagNode (tagHs "Pair") $ ESeq [avC]
    asYAML ref = do
        val <- fakeEval $ readRef ref
        svC <- asYAML val
        liftIO $ print "====>"
        liftIO $ print svC
        fail ("Not implemented: asYAML \"" ++ showType (refType ref) ++ "\"")
    fromYAML MkNode{n_tag=Just s, n_elem=ESeq [node]}
        | s == packBuf "tag:hs:VCode"   =
            fmap (MkRef . ICode) (fromYAML node :: IO VCode)
        | s == packBuf "tag:hs:VScalar" =
            fmap (MkRef . IScalar) (fromYAML node :: IO VScalar)
        | s == packBuf "tag:hs:Pair"    =
            fmap pairRef (fromYAML node :: IO VPair)
        | s == packBuf "tag:hs:IScalar" = newV newScalar
        | s == packBuf "tag:hs:Array"   = newV newArray
        | s == packBuf "tag:hs:Hash"    = newV newHash
        where newV f = fmap MkRef (f =<< fromYAML node)
    fromYAML node = fail $ "Unhandled YAML node: " ++ show node
instance YAML IHash where
     asYAML x = do
         l      <- liftIO $ H.toList x
         asYAMLmapBuf "IHash" (map (\(k, v) -> (k, asYAML v)) l)
     fromYAML node = do
         l  <- fromYAMLmapBuf node
         l' <- hashList l
         return l'

instance YAML ID where
    asYAML x = asYAML (idBuf x)
    fromYAML x = do
        buf <- fromYAML x
        bufToID buf
 
instance Perl5 ID where
    showPerl5 x = showPerl5 (cast x :: ByteString)
instance JSON ID where
    showJSON x = showJSON (cast x :: ByteString)

instance YAML Var where
    asYAML x = asYAML (cast x :: String)
    fromYAML = fmap (cast :: String -> Var) . fromYAML
 
instance Perl5 Var where
    showPerl5 x = showPerl5 (cast x :: String)
instance JSON Var where
    showJSON x = showJSON (cast x :: String)

instance YAML (Set Val) where
    asYAML = asYAML . Set.toAscList
    fromYAML = fmap Set.fromAscList . fromYAML 

instance YAML VControl
instance YAML VThread
instance YAML ClassTree
instance YAML Dynamic
instance YAML ProcessHandle
instance YAML Regex
instance YAML Unique
instance YAML VComplex
instance YAML VHandle
instance YAML VOpaque
instance YAML VSocket
instance YAML PerlSV

instance Perl5 Exp where
    showPerl5 _ = "(undef)"
instance JSON Exp where
    showJSON _ = "null"

-- Non-canonical serialization... needs work
instance (Show (TVar a)) => Perl5 (TVar a) where
    showPerl5 _ = "(warn '<ref>')"
instance (Show (TVar a)) => JSON (TVar a) where
    showJSON _ = "null"

instance Perl5 Val where
    showPerl5 (VUndef) = showP5Class "VUndef"
    showPerl5 (VBool aa) = showP5ArrayObj "VBool" [showPerl5 aa]
    showPerl5 (VInt aa) = showP5ArrayObj "VInt" [showPerl5 aa]
    showPerl5 (VRat aa) = showP5ArrayObj "VRat" [showPerl5 aa]
    showPerl5 (VNum aa) = showP5ArrayObj "VNum" [showPerl5 aa]
    showPerl5 (VStr aa) = showP5ArrayObj "VStr" [showPerl5 aa]
    showPerl5 (VList aa) = showP5ArrayObj "VList" [showPerl5 aa]
    showPerl5 (VType aa) = showP5ArrayObj "VType" [showPerl5 aa]
    showPerl5 (VCode{}) = showP5Class "VUndef" -- XXX - fix the END block!

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance YAML VSubst where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkSubst" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkSubst (fromYAML aa) (fromYAML ab)
	"MkTrans" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkTrans (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkSubst","MkTrans"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkSubst aa ab) = asYAMLseq "MkSubst" [asYAML aa, asYAML ab]
    asYAML (MkTrans aa ab) = asYAMLseq "MkTrans" [asYAML aa, asYAML ab]

instance YAML VThunk where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkThunk" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkThunk (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkThunk"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkThunk aa ab) = asYAMLseq "MkThunk" [asYAML aa, asYAML ab]

instance YAML VProcess where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkProcess" -> do
	    let ESeq [aa] = e
	    liftM MkProcess (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkProcess"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkProcess aa) = asYAMLseq "MkProcess" [asYAML aa]

instance YAML VRule where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkRulePCRE" -> do
	    let liftM6 f m1 m2 m3 m4 m5 m6 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6)}
	    let ESeq [aa, ab, ac, ad, ae, af] = e
	    liftM6 MkRulePCRE (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af)
	"MkRulePGE" -> do
	    let ESeq [aa, ab, ac, ad] = e
	    liftM4 MkRulePGE (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkRulePCRE","MkRulePGE"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkRulePCRE aa ab ac ad ae af) = asYAMLseq "MkRulePCRE"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af]
    asYAML (MkRulePGE aa ab ac ad) = asYAMLseq "MkRulePGE"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad]

instance YAML Val where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"VUndef" -> do
	    return VUndef
	"VBool" -> do
	    let ESeq [aa] = e
	    liftM VBool (fromYAML aa)
	"VInt" -> do
	    let ESeq [aa] = e
	    liftM VInt (fromYAML aa)
	"VRat" -> do
	    let ESeq [aa] = e
	    liftM VRat (fromYAML aa)
	"VNum" -> do
	    let ESeq [aa] = e
	    liftM VNum (fromYAML aa)
	"VComplex" -> do
	    let ESeq [aa] = e
	    liftM VComplex (fromYAML aa)
	"VStr" -> do
	    let ESeq [aa] = e
	    liftM VStr (fromYAML aa)
	"VList" -> do
	    let ESeq [aa] = e
	    liftM VList (fromYAML aa)
	"VType" -> do
	    let ESeq [aa] = e
	    liftM VType (fromYAML aa)
	"VJunc" -> do
	    let ESeq [aa] = e
	    liftM VJunc (fromYAML aa)
	"VError" -> do
	    let ESeq [aa, ab] = e
	    liftM2 VError (fromYAML aa) (fromYAML ab)
	"VControl" -> do
	    let ESeq [aa] = e
	    liftM VControl (fromYAML aa)
	"VRef" -> do
	    let ESeq [aa] = e
	    liftM VRef (fromYAML aa)
	"VCode" -> do
	    let ESeq [aa] = e
	    liftM VCode (fromYAML aa)
	"VBlock" -> do
	    let ESeq [aa] = e
	    liftM VBlock (fromYAML aa)
	"VHandle" -> do
	    let ESeq [aa] = e
	    liftM VHandle (fromYAML aa)
	"VSocket" -> do
	    let ESeq [aa] = e
	    liftM VSocket (fromYAML aa)
	"VThread" -> do
	    let ESeq [aa] = e
	    liftM VThread (fromYAML aa)
	"VProcess" -> do
	    let ESeq [aa] = e
	    liftM VProcess (fromYAML aa)
	"VRule" -> do
	    let ESeq [aa] = e
	    liftM VRule (fromYAML aa)
	"VSubst" -> do
	    let ESeq [aa] = e
	    liftM VSubst (fromYAML aa)
	"VMatch" -> do
	    let ESeq [aa] = e
	    liftM VMatch (fromYAML aa)
	"VObject" -> do
	    let ESeq [aa] = e
	    liftM VObject (fromYAML aa)
	"VOpaque" -> do
	    let ESeq [aa] = e
	    liftM VOpaque (fromYAML aa)
	"PerlSV" -> do
	    let ESeq [aa] = e
	    liftM PerlSV (fromYAML aa)
	"VV" -> do
	    let ESeq [aa] = e
	    liftM VV (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["VUndef","VBool","VInt","VRat","VNum","VComplex","VStr","VList","VType","VJunc","VError","VControl","VRef","VCode","VBlock","VHandle","VSocket","VThread","VProcess","VRule","VSubst","VMatch","VObject","VOpaque","PerlSV","VV"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (VUndef) = asYAMLcls "VUndef"
    asYAML (VBool aa) = asYAMLseq "VBool" [asYAML aa]
    asYAML (VInt aa) = asYAMLseq "VInt" [asYAML aa]
    asYAML (VRat aa) = asYAMLseq "VRat" [asYAML aa]
    asYAML (VNum aa) = asYAMLseq "VNum" [asYAML aa]
    asYAML (VComplex aa) = asYAMLseq "VComplex" [asYAML aa]
    asYAML (VStr aa) = asYAMLseq "VStr" [asYAML aa]
    asYAML (VList aa) = asYAMLseq "VList" [asYAML aa]
    asYAML (VType aa) = asYAMLseq "VType" [asYAML aa]
    asYAML (VJunc aa) = asYAMLseq "VJunc" [asYAML aa]
    asYAML (VError aa ab) = asYAMLseq "VError" [asYAML aa, asYAML ab]
    asYAML (VControl aa) = asYAMLseq "VControl" [asYAML aa]
    asYAML (VRef aa) = asYAMLseq "VRef" [asYAML aa]
    asYAML (VCode aa) = asYAMLseq "VCode" [asYAML aa]
    asYAML (VBlock aa) = asYAMLseq "VBlock" [asYAML aa]
    asYAML (VHandle aa) = asYAMLseq "VHandle" [asYAML aa]
    asYAML (VSocket aa) = asYAMLseq "VSocket" [asYAML aa]
    asYAML (VThread aa) = asYAMLseq "VThread" [asYAML aa]
    asYAML (VProcess aa) = asYAMLseq "VProcess" [asYAML aa]
    asYAML (VRule aa) = asYAMLseq "VRule" [asYAML aa]
    asYAML (VSubst aa) = asYAMLseq "VSubst" [asYAML aa]
    asYAML (VMatch aa) = asYAMLseq "VMatch" [asYAML aa]
    asYAML (VObject aa) = asYAMLseq "VObject" [asYAML aa]
    asYAML (VOpaque aa) = asYAMLseq "VOpaque" [asYAML aa]
    asYAML (PerlSV aa) = asYAMLseq "PerlSV" [asYAML aa]
    asYAML (VV aa) = asYAMLseq "VV" [asYAML aa]

instance YAML SubType where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"SubMethod" -> do
	    return SubMethod
	"SubCoroutine" -> do
	    return SubCoroutine
	"SubMacro" -> do
	    return SubMacro
	"SubRoutine" -> do
	    return SubRoutine
	"SubBlock" -> do
	    return SubBlock
	"SubPointy" -> do
	    return SubPointy
	"SubPrim" -> do
	    return SubPrim
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["SubMethod","SubCoroutine","SubMacro","SubRoutine","SubBlock","SubPointy","SubPrim"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (SubMethod) = asYAMLcls "SubMethod"
    asYAML (SubCoroutine) = asYAMLcls "SubCoroutine"
    asYAML (SubMacro) = asYAMLcls "SubMacro"
    asYAML (SubRoutine) = asYAMLcls "SubRoutine"
    asYAML (SubBlock) = asYAMLcls "SubBlock"
    asYAML (SubPointy) = asYAMLcls "SubPointy"
    asYAML (SubPrim) = asYAMLcls "SubPrim"

instance JSON SubType where
    showJSON (SubMethod) = showJSScalar "SubMethod"
    showJSON (SubCoroutine) = showJSScalar "SubCoroutine"
    showJSON (SubMacro) = showJSScalar "SubMacro"
    showJSON (SubRoutine) = showJSScalar "SubRoutine"
    showJSON (SubBlock) = showJSScalar "SubBlock"
    showJSON (SubPointy) = showJSScalar "SubPointy"
    showJSON (SubPrim) = showJSScalar "SubPrim"

instance Perl5 SubType where
    showPerl5 (SubMethod) = showP5Class "SubMethod"
    showPerl5 (SubCoroutine) = showP5Class "SubCoroutine"
    showPerl5 (SubMacro) = showP5Class "SubMacro"
    showPerl5 (SubRoutine) = showP5Class "SubRoutine"
    showPerl5 (SubBlock) = showP5Class "SubBlock"
    showPerl5 (SubPointy) = showP5Class "SubPointy"
    showPerl5 (SubPrim) = showP5Class "SubPrim"

instance YAML Param where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkOldParam" -> do
	    let liftM9 f m1 m2 m3 m4 m5 m6 m7 m8 m9 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9)}
	    let ESeq [aa, ab, ac, ad, ae, af, ag, ah, ai] = e
	    liftM9 MkOldParam (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af) (fromYAML ag) (fromYAML ah) (fromYAML ai)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkOldParam"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkOldParam aa ab ac ad ae af ag ah ai) =
	   asYAMLseq "MkOldParam"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af,
	    asYAML ag, asYAML ah, asYAML ai]

instance Perl5 Param where
    showPerl5 (MkOldParam aa ab ac ad ae af ag ah ai) =
	      showP5HashObj "MkOldParam"
	      [("isInvocant", showPerl5 aa) , ("isOptional", showPerl5 ab) ,
	       ("isNamed", showPerl5 ac) , ("isLValue", showPerl5 ad) ,
	       ("isWritable", showPerl5 ae) , ("isLazy", showPerl5 af) ,
	       ("paramName", showPerl5 ag) , ("paramContext", showPerl5 ah) ,
	       ("paramDefault", showPerl5 ai)]

instance JSON Param where
    showJSON (MkOldParam aa ab ac ad ae af ag ah ai) =
	     showJSHashObj "MkOldParam"
	     [("isInvocant", showJSON aa), ("isOptional", showJSON ab),
	      ("isNamed", showJSON ac), ("isLValue", showJSON ad),
	      ("isWritable", showJSON ae), ("isLazy", showJSON af),
	      ("paramName", showJSON ag), ("paramContext", showJSON ah),
	      ("paramDefault", showJSON ai)]

instance YAML SubAssoc where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"ANil" -> do
	    return ANil
	"AIrrelevantToParsing" -> do
	    return AIrrelevantToParsing
	"A_left" -> do
	    return A_left
	"A_right" -> do
	    return A_right
	"A_non" -> do
	    return A_non
	"A_chain" -> do
	    return A_chain
	"A_list" -> do
	    return A_list
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["ANil","AIrrelevantToParsing","A_left","A_right","A_non","A_chain","A_list"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (ANil) = asYAMLcls "ANil"
    asYAML (AIrrelevantToParsing) = asYAMLcls "AIrrelevantToParsing"
    asYAML (A_left) = asYAMLcls "A_left"
    asYAML (A_right) = asYAMLcls "A_right"
    asYAML (A_non) = asYAMLcls "A_non"
    asYAML (A_chain) = asYAMLcls "A_chain"
    asYAML (A_list) = asYAMLcls "A_list"

instance JSON SubAssoc where
    showJSON (ANil) = showJSScalar "ANil"
    showJSON (AIrrelevantToParsing) =
	     showJSScalar "AIrrelevantToParsing"
    showJSON (A_left) = showJSScalar "A_left"
    showJSON (A_right) = showJSScalar "A_right"
    showJSON (A_non) = showJSScalar "A_non"
    showJSON (A_chain) = showJSScalar "A_chain"
    showJSON (A_list) = showJSScalar "A_list"

instance Perl5 SubAssoc where
    showPerl5 (ANil) = showP5Class "ANil"
    showPerl5 (AIrrelevantToParsing) =
	      showP5Class "AIrrelevantToParsing"
    showPerl5 (A_left) = showP5Class "A::left"
    showPerl5 (A_right) = showP5Class "A::right"
    showPerl5 (A_non) = showP5Class "A::non"
    showPerl5 (A_chain) = showP5Class "A::chain"
    showPerl5 (A_list) = showP5Class "A::list"

instance YAML VCode where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkCode" -> do
	    let liftM23 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; x10 <- m10; x11 <- m11; x12 <- m12; x13 <- m13; x14 <- m14; x15 <- m15; x16 <- m16; x17 <- m17; x18 <- m18; x19 <- m19; x20 <- m20; x21 <- m21; x22 <- m22; x23 <- m23; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23)}
	    let ESeq [aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw] = e
	    liftM23 MkCode (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af) (fromYAML ag) (fromYAML ah) (fromYAML ai) (fromYAML aj) (fromYAML ak) (fromYAML al) (fromYAML am) (fromYAML an) (fromYAML ao) (fromYAML ap) (fromYAML aq) (fromYAML ar) (fromYAML as) (fromYAML at) (fromYAML au) (fromYAML av) (fromYAML aw)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkCode"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkCode aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
	    ar as at au av aw)
	   =
	   asYAMLseq "MkCode"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af,
	    asYAML ag, asYAML ah, asYAML ai, asYAML aj, asYAML ak, asYAML al,
	    asYAML am, asYAML an, asYAML ao, asYAML ap, asYAML aq, asYAML ar,
	    asYAML as, asYAML at, asYAML au, asYAML av, asYAML aw]

instance YAML Ann where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Cxt" -> do
	    let ESeq [aa] = e
	    liftM Cxt (fromYAML aa)
	"Pos" -> do
	    let ESeq [aa] = e
	    liftM Pos (fromYAML aa)
	"Prag" -> do
	    let ESeq [aa] = e
	    liftM Prag (fromYAML aa)
	"Decl" -> do
	    let ESeq [aa] = e
	    liftM Decl (fromYAML aa)
	"Parens" -> do
	    return Parens
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Cxt","Pos","Prag","Decl","Parens"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Cxt aa) = asYAMLseq "Cxt" [asYAML aa]
    asYAML (Pos aa) = asYAMLseq "Pos" [asYAML aa]
    asYAML (Prag aa) = asYAMLseq "Prag" [asYAML aa]
    asYAML (Decl aa) = asYAMLseq "Decl" [asYAML aa]
    asYAML (Parens) = asYAMLcls "Parens"

instance YAML Exp where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"Noop" -> do
	    return Noop
	"App" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 App (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"Syn" -> do
	    let ESeq [aa, ab] = e
	    liftM2 Syn (fromYAML aa) (fromYAML ab)
	"Ann" -> do
	    let ESeq [aa, ab] = e
	    liftM2 Ann (fromYAML aa) (fromYAML ab)
	"Pad" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 Pad (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"Sym" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 Sym (fromYAML aa) (fromYAML ab) (fromYAML ac)
	"Stmts" -> do
	    let ESeq [aa, ab] = e
	    liftM2 Stmts (fromYAML aa) (fromYAML ab)
	"Prim" -> do
	    let ESeq [aa] = e
	    liftM Prim (fromYAML aa)
	"Val" -> do
	    let ESeq [aa] = e
	    liftM Val (fromYAML aa)
	"Var" -> do
	    let ESeq [aa] = e
	    liftM Var (fromYAML aa)
	"NonTerm" -> do
	    let ESeq [aa] = e
	    liftM NonTerm (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["Noop","App","Syn","Ann","Pad","Sym","Stmts","Prim","Val","Var","NonTerm"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (Noop) = asYAMLcls "Noop"
    asYAML (App aa ab ac) = asYAMLseq "App"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (Syn aa ab) = asYAMLseq "Syn" [asYAML aa, asYAML ab]
    asYAML (Ann aa ab) = asYAMLseq "Ann" [asYAML aa, asYAML ab]
    asYAML (Pad aa ab ac) = asYAMLseq "Pad"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (Sym aa ab ac) = asYAMLseq "Sym"
	   [asYAML aa, asYAML ab, asYAML ac]
    asYAML (Stmts aa ab) = asYAMLseq "Stmts" [asYAML aa, asYAML ab]
    asYAML (Prim aa) = asYAMLseq "Prim" [asYAML aa]
    asYAML (Val aa) = asYAMLseq "Val" [asYAML aa]
    asYAML (Var aa) = asYAMLseq "Var" [asYAML aa]
    asYAML (NonTerm aa) = asYAMLseq "NonTerm" [asYAML aa]

instance YAML InitDat where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkInitDat" -> do
	    let ESeq [aa] = e
	    liftM MkInitDat (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkInitDat"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkInitDat aa) = asYAMLseq "MkInitDat" [asYAML aa]

instance YAML PadEntry where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkEntry" -> do
	    let ESeq [aa] = e
	    liftM MkEntry (fromYAML aa)
	"MkEntryMulti" -> do
	    let ESeq [aa] = e
	    liftM MkEntryMulti (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkEntry","MkEntryMulti"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkEntry aa) = asYAMLseq "MkEntry" [asYAML aa]
    asYAML (MkEntryMulti aa) = asYAMLseq "MkEntryMulti" [asYAML aa]

instance YAML IHashEnv where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkHashEnv" -> do
	    return MkHashEnv
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkHashEnv"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkHashEnv) = asYAMLcls "MkHashEnv"

instance YAML IScalarCwd where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkScalarCwd" -> do
	    return MkScalarCwd
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkScalarCwd"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkScalarCwd) = asYAMLcls "MkScalarCwd"

instance YAML ObjectId where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkObjectId" -> do
	    let ESeq [aa] = e
	    liftM MkObjectId (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkObjectId"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkObjectId aa) = asYAMLseq "MkObjectId" [asYAML aa]

instance YAML VObject where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkObject" -> do
	    let ESeq [aa, ab, ac, ad] = e
	    liftM4 MkObject (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkObject"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkObject aa ab ac ad) = asYAMLseq "MkObject"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad]

instance YAML VMatch where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkMatch" -> do
	    let liftM6 f m1 m2 m3 m4 m5 m6 = do
		{x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6)}
	    let ESeq [aa, ab, ac, ad, ae, af] = e
	    liftM6 MkMatch (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae) (fromYAML af)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkMatch"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkMatch aa ab ac ad ae af) = asYAMLseq "MkMatch"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae, asYAML af]

instance YAML CompUnit where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkCompUnit" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkCompUnit (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkCompUnit"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkCompUnit aa ab ac) = asYAMLseq "MkCompUnit"
	   [asYAML aa, asYAML ab, asYAML ac]

instance YAML VJunc where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkJunc" -> do
	    let ESeq [aa, ab, ac] = e
	    liftM3 MkJunc (fromYAML aa) (fromYAML ab) (fromYAML ac)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkJunc"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkJunc aa ab ac) = asYAMLseq "MkJunc"
	   [asYAML aa, asYAML ab, asYAML ac]

instance YAML JuncType where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"JAny" -> do
	    return JAny
	"JAll" -> do
	    return JAll
	"JNone" -> do
	    return JNone
	"JOne" -> do
	    return JOne
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["JAny","JAll","JNone","JOne"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (JAny) = asYAMLcls "JAny"
    asYAML (JAll) = asYAMLcls "JAll"
    asYAML (JNone) = asYAMLcls "JNone"
    asYAML (JOne) = asYAMLcls "JOne"

instance YAML Scope where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"SState" -> do
	    return SState
	"SLet" -> do
	    return SLet
	"STemp" -> do
	    return STemp
	"SEnv" -> do
	    return SEnv
	"SMy" -> do
	    return SMy
	"SOur" -> do
	    return SOur
	"SGlobal" -> do
	    return SGlobal
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["SState","SLet","STemp","SEnv","SMy","SOur","SGlobal"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (SState) = asYAMLcls "SState"
    asYAML (SLet) = asYAMLcls "SLet"
    asYAML (STemp) = asYAMLcls "STemp"
    asYAML (SEnv) = asYAMLcls "SEnv"
    asYAML (SMy) = asYAMLcls "SMy"
    asYAML (SOur) = asYAMLcls "SOur"
    asYAML (SGlobal) = asYAMLcls "SGlobal"

instance JSON Scope where
    showJSON (SState) = showJSScalar "SState"
    showJSON (SLet) = showJSScalar "SLet"
    showJSON (STemp) = showJSScalar "STemp"
    showJSON (SEnv) = showJSScalar "SEnv"
    showJSON (SMy) = showJSScalar "SMy"
    showJSON (SOur) = showJSScalar "SOur"
    showJSON (SGlobal) = showJSScalar "SGlobal"

instance Perl5 Scope where
    showPerl5 (SState) = showP5Class "SState"
    showPerl5 (SLet) = showP5Class "SLet"
    showPerl5 (STemp) = showP5Class "STemp"
    showPerl5 (SEnv) = showP5Class "SEnv"
    showPerl5 (SMy) = showP5Class "SMy"
    showPerl5 (SOur) = showP5Class "SOur"
    showPerl5 (SGlobal) = showP5Class "SGlobal"

instance YAML Pad where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkPad" -> do
	    let ESeq [aa] = e
	    liftM MkPad (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkPad"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkPad aa) = asYAMLseq "MkPad" [asYAML aa]

instance YAML Pos where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkPos" -> do
	    let ESeq [aa, ab, ac, ad, ae] = e
	    liftM5 MkPos (fromYAML aa) (fromYAML ab) (fromYAML ac) (fromYAML ad) (fromYAML ae)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkPos"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkPos aa ab ac ad ae) = asYAMLseq "MkPos"
	   [asYAML aa, asYAML ab, asYAML ac, asYAML ad, asYAML ae]

instance JSON Pos where
    showJSON (MkPos aa ab ac ad ae) = showJSHashObj "MkPos"
	     [("posName", showJSON aa), ("posBeginLine", showJSON ab),
	      ("posBeginColumn", showJSON ac), ("posEndLine", showJSON ad),
	      ("posEndColumn", showJSON ae)]

instance Perl5 Pos where
    showPerl5 (MkPos aa ab ac ad ae) = showP5HashObj "MkPos"
	      [("posName", showPerl5 aa) , ("posBeginLine", showPerl5 ab) ,
	       ("posBeginColumn", showPerl5 ac) , ("posEndLine", showPerl5 ad) ,
	       ("posEndColumn", showPerl5 ae)]

instance YAML Type where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkType" -> do
	    let ESeq [aa] = e
	    liftM MkType (fromYAML aa)
	"TypeOr" -> do
	    let ESeq [aa, ab] = e
	    liftM2 TypeOr (fromYAML aa) (fromYAML ab)
	"TypeAnd" -> do
	    let ESeq [aa, ab] = e
	    liftM2 TypeAnd (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkType","TypeOr","TypeAnd"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkType aa) = asYAMLseq "MkType" [asYAML aa]
    asYAML (TypeOr aa ab) = asYAMLseq "TypeOr" [asYAML aa, asYAML ab]
    asYAML (TypeAnd aa ab) = asYAMLseq "TypeAnd" [asYAML aa, asYAML ab]

instance JSON Type where
    showJSON (MkType aa) = showJSArrayObj "MkType" [showJSON aa]
    showJSON (TypeOr aa ab) = showJSArrayObj "TypeOr"
	     [showJSON aa, showJSON ab]
    showJSON (TypeAnd aa ab) = showJSArrayObj "TypeAnd"
	     [showJSON aa, showJSON ab]

instance Perl5 Type where
    showPerl5 (MkType aa) = showP5ArrayObj "MkType" [showPerl5 aa]
    showPerl5 (TypeOr aa ab) = showP5ArrayObj "TypeOr"
	      [showPerl5 aa , showPerl5 ab]
    showPerl5 (TypeAnd aa ab) = showP5ArrayObj "TypeAnd"
	      [showPerl5 aa , showPerl5 ab]

instance YAML Cxt where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"CxtVoid" -> do
	    return CxtVoid
	"CxtItem" -> do
	    let ESeq [aa] = e
	    liftM CxtItem (fromYAML aa)
	"CxtSlurpy" -> do
	    let ESeq [aa] = e
	    liftM CxtSlurpy (fromYAML aa)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["CxtVoid","CxtItem","CxtSlurpy"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (CxtVoid) = asYAMLcls "CxtVoid"
    asYAML (CxtItem aa) = asYAMLseq "CxtItem" [asYAML aa]
    asYAML (CxtSlurpy aa) = asYAMLseq "CxtSlurpy" [asYAML aa]

instance JSON Cxt where
    showJSON (CxtVoid) = showJSScalar "CxtVoid"
    showJSON (CxtItem aa) = showJSArrayObj "CxtItem" [showJSON aa]
    showJSON (CxtSlurpy aa) = showJSArrayObj "CxtSlurpy" [showJSON aa]

instance Perl5 Cxt where
    showPerl5 (CxtVoid) = showP5Class "CxtVoid"
    showPerl5 (CxtItem aa) = showP5ArrayObj "CxtItem" [showPerl5 aa]
    showPerl5 (CxtSlurpy aa) = showP5ArrayObj "CxtSlurpy"
	      [showPerl5 aa]

instance JSON Val where
    showJSON (VUndef) = showJSScalar "VUndef"
    showJSON (VBool aa) = showJSArrayObj "VBool" [showJSON aa]
    showJSON (VInt aa) = showJSArrayObj "VInt" [showJSON aa]
    showJSON (VRat aa) = showJSArrayObj "VRat" [showJSON aa]
    showJSON (VNum aa) = showJSArrayObj "VNum" [showJSON aa]
    showJSON (VStr aa) = showJSArrayObj "VStr" [showJSON aa]
    showJSON (VList aa) = showJSArrayObj "VList" [showJSON aa]
    showJSON (VType aa) = showJSArrayObj "VType" [showJSON aa]

instance YAML Pragma where
    fromYAML MkNode{n_tag=Just t, n_elem=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackBuf t = case tag of
	"MkPrag" -> do
	    let ESeq [aa, ab] = e
	    liftM2 MkPrag (fromYAML aa) (fromYAML ab)
	_ -> fail $ "unhandled tag: " ++ show t ++ ", expecting " ++ show ["MkPrag"] ++ " in node " ++ show e
    fromYAML _ = fail "no tag found"
    asYAML (MkPrag aa ab) = asYAMLseq "MkPrag" [asYAML aa, asYAML ab]

instance JSON Pragma where
    showJSON (MkPrag aa ab) = showJSHashObj "MkPrag"
	     [("pragName", showJSON aa), ("pragDat", showJSON ab)]

instance Perl5 Pragma where
    showPerl5 (MkPrag aa ab) = showP5HashObj "MkPrag"
	      [("pragName", showPerl5 aa) , ("pragDat", showPerl5 ab)]

--  Imported from other files :-


#endif
