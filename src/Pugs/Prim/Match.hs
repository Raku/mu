{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.Prim.Match (
    op2Match, rxSplit, rxSplit_n, matchFromMR, pkgParents
) where
import Pugs.Internals
import Pugs.Embed
import Pugs.AST
import Pugs.Types
import Pugs.Config
import qualified RRegex.PCRE as PCRE
import qualified Data.Map as Map
import qualified Data.Array as Array

doMatch :: String -> VRule -> Eval VMatch
doMatch cs MkRulePGE{ rxRule = re } = do
    let pwd1 = getConfig "installarchlib" ++ "/CORE/pugs/pge"
        pwd2 = getConfig "sourcedir" ++ "/src/pge"
    hasSrc <- liftIO $ doesDirectoryExist pwd2
    let pwd = if hasSrc then pwd2 else pwd1
    glob    <- askGlobal
    let syms = [ (name, tvar) | (('<':'*':name), [(_, tvar)]) <- padToList glob ]
    subrules <- forM syms $ \(name, tvar) -> do
        ref  <- liftSTM $ readTVar tvar
        (VRule rule) <- fromVal =<< readRef ref
        return (name, rxRule rule)
    pge <- liftIO $ evalPGE pwd (encodeUTF8 cs) (encodeUTF8 re) subrules
            `catch` (\e -> return $ ioeGetErrorString e)
    rv  <- tryIO Nothing $ fmap Just (readIO $ decodeUTF8 pge)
    let matchToVal PGE_Fail = VMatch mkMatchFail
        matchToVal (PGE_Array ms) = VList (map matchToVal ms)
        matchToVal (PGE_Match from to pos named) = VMatch $
            mkMatchOk from to substr pos' named'
            where
            substr  = genericTake (to - from) (genericDrop from cs)
            pos'    = map matchToVal pos
            named'  = Map.map matchToVal $ Map.fromList named
    case rv of
        Just m  -> fromVal (matchToVal m)
        Nothing -> do
            liftIO $ putStrLn ("*** Cannot parse PGE: " ++ re ++ "\n*** Error: " ++ pge)
            return mkMatchFail

doMatch csChars MkRulePCRE{ rxRegex = re } = do
    rv <- liftIO $ PCRE.execute re csBytes 0
    if isNothing rv then return mkMatchFail else do
    let ((fromBytes, lenBytes):subs) = Array.elems (fromJust rv)
        substr str from len = take len (drop from str)
        subsMatch = [
            VMatch $ mkMatchOk
                fChars (fChars + lChars)
                (substr csChars fChars lChars)
                [] Map.empty
            | (fBytes, lBytes) <- subs
            , let fChars = chars $ take fBytes csBytes
            , let lChars = chars $ substr csBytes fBytes lBytes
            ]
        fromChars = chars $ take fromBytes csBytes
        lenChars  = chars $ substr csBytes fromBytes lenBytes
        chars = genericLength . decodeUTF8

    return $ mkMatchOk fromChars (fromChars + lenChars) (substr csChars fromChars lenChars) subsMatch Map.empty
    where
    csBytes = encodeUTF8 csChars

matchFromMR :: MatchResult Char -> Val
matchFromMR mr = VMatch $ mkMatchOk 0 0 (decodeUTF8 all) subsMatch Map.empty
    where
    (all:subs) = elems $ mrSubs mr
    subsMatch = [ VMatch $ mkMatchOk 0 0 (decodeUTF8 sub) [] Map.empty | sub <- subs ]

-- XXX - need to generalise this
op2Match :: Val -> Val -> Eval Val

op2Match x (VRef (MkRef (IScalar sv))) | scalar_iType sv == mkType "Scalar::Const" = do
    y' <- scalar_fetch' sv
    op2Match x y'

op2Match x (VRef y) = do
    y' <- readRef y
    op2Match x y'

op2Match x y@(VObject MkObject{ objType = MkType "Class" } ) = do
    fetch   <- doHash y hash_fetchVal
    name    <- fromVal =<< fetch "name"
    op2Match x (VType (MkType name))

op2Match x (VSubst (rx, subst)) | rxGlobal rx = do
    str         <- fromVal x
    (str', cnt) <- doReplace str 0
    if cnt == 0 then return (VBool False) else do
    ref     <- fromVal x
    writeRef ref $ VStr str'
    return $ castV cnt
    where
    doReplace :: String -> Int -> Eval (String, Int)
    doReplace str ok = do
        match <- str `doMatch` rx
        if not (matchOk match) then return (str, ok) else do
        glob    <- askGlobal
        matchSV <- findSymRef "$/" glob
        writeRef matchSV (VMatch match)
        str'    <- fromVal =<< evalExp subst
        (after', ok') <- doReplace (genericDrop (matchTo match) str) (ok + 1)
        return (concat [genericTake (matchFrom match) str, str', after'], ok')

op2Match x (VSubst (rx, subst)) = do
    str     <- fromVal x
    ref     <- fromVal x
    match   <- str `doMatch` rx
    if not (matchOk match) then return (VBool False) else do
    glob    <- askGlobal
    matchSV <- findSymRef "$/" glob
    writeRef matchSV (VMatch match)
    str'    <- fromVal =<< evalExp subst
    writeRef ref . VStr $ concat
        [ genericTake (matchFrom match) str
        , str'
        , genericDrop (matchTo match) str
        ]
    return $ VBool True

op2Match x (VRule rx) | rxGlobal rx = do
    str     <- fromVal x
    rv      <- matchOnce str
    cxt	    <- asks envContext
    if (not $ isSlurpyCxt cxt)
	then return (VInt $ genericLength rv)
	else if rxStringify rx
	    then do
                strs <- mapM fromVal rv
                return (VList $ map VStr strs)
	    else return (VList rv)
    where
    matchOnce :: String -> Eval [Val]
    matchOnce str = do
        match <- str `doMatch` rx
        if not (matchOk match) then return [] else do
        rest <- matchOnce (genericDrop (matchTo match) str)
        return $ matchSubPos match ++ rest

op2Match x (VRule rx) = do
    str     <- fromVal x
    match   <- str `doMatch` rx
    glob    <- askGlobal
    matchSV <- findSymRef "$/" glob
    writeRef matchSV (VMatch match)
    ifListContext
        (return $ VList (matchSubPos match))
        (return $ VMatch match)

op2Match (VType typ) (VType t) = do
    typs <- pkgParents (showType typ)
    return . VBool $ showType t `elem` typs

op2Match x y@(VType _) = do
    typ <- fromVal x
    op2Match (VType typ) y

{-
op2Match (VRef x) y = do
    x' <- readRef x
    op2Match x' y
-}

op2Match x y = do
    op2Cmp (fromVal :: Val -> Eval VStr) (==) x y

op2Cmp :: (a -> Eval b) -> (b -> b -> VBool) -> a -> a -> Eval Val
op2Cmp f cmp x y = do
    x' <- f x
    y' <- f y
    return $ VBool $ x' `cmp` y'

rxSplit :: VRule -> String -> Eval [Val]
rxSplit _  [] = return []
rxSplit rx str = do
    match <- str `doMatch` rx
    if not (matchOk match) then return [VStr str] else do
    if matchFrom match == matchTo match
        then do
            let (c:cs) = str
            rest <- rxSplit rx (cs)
            return (VStr [c]:rest)
        else do
            let before = genericTake (matchFrom match) str
                after  = genericDrop (matchTo match) str
            rest <- rxSplit rx after
            return $ (VStr before:matchSubPos match) ++ rest

-- duplicated for now, pending über-Haskell-fu

rxSplit_n :: VRule -> String -> Int -> Eval [Val]
rxSplit_n _ [] _ = return []
rxSplit_n rx str n = do
    match <- str `doMatch` rx
    if or [ ( n == 1 ), ( not (matchOk match) ) ] then return [VStr str] else do
    if matchFrom match == matchTo match
        then do
            let (c:cs) = str
            rest <- rxSplit_n rx (cs) (n-1)
            return (VStr [c]:rest)
        else do
            let before = genericTake (matchFrom match) str
                after  = genericDrop (matchTo match) str
            rest <- rxSplit_n rx after (n-1)
            return $ (VStr before:matchSubPos match) ++ rest

pkgParents :: VStr -> Eval [VStr]
pkgParents pkg = do
    ref     <- readVar (':':'*':pkg)
    if ref == undef then return [] else do
    meta    <- readRef =<< fromVal ref
    fetch   <- doHash meta hash_fetchVal
    attrs   <- fromVal =<< fetch "traits"
    pkgs    <- mapM pkgParents attrs
    return $ nub (pkg:concat pkgs)

