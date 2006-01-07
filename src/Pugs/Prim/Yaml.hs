{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.Prim.Yaml (
  evalYaml, dumpYaml
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Pretty
import Pugs.Types
import Data.Yaml.Syck
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

evalYaml :: Val -> Eval Val
evalYaml cv = do
    str     <- fromVal cv
    rv      <- liftIO (parseYaml $ encodeUTF8 str)
    case rv of
        Left err            -> fail $ "YAML Parse Error: " ++ err
        Right Nothing       -> return undef
        Right (Just node)   -> fromYaml node

fromYaml :: YamlNode -> Eval Val
fromYaml YamlNil = return VUndef
fromYaml (YamlStr str) = return $ VStr (decodeUTF8 str)
fromYaml (YamlSeq nodes) = do
    vals    <- mapM fromYaml nodes
    av      <- liftSTM $ newTVar $
        IntMap.fromAscList ([0..] `zip` map lazyScalar vals)
    return $ VRef (arrayRef av)
fromYaml (YamlMap _ nodes) = do
    vals    <- forM nodes $ \(keyNode, valNode) -> do
        key <- fromVal =<< fromYaml keyNode
        val <- newScalar =<< fromYaml valNode
        return (key, val)
    hv      <- liftSTM $ (newTVar (Map.fromList vals) :: STM IHash)
    -- XXX: if YamlMap (Just "!perl/":type) nodes then mkObject etc.
    return $ VRef (hashRef hv)

dumpYaml :: Int -> Val -> Eval Val
dumpYaml limit v = do
    obj  <- toYaml limit v
    rv   <- liftIO (emitYaml obj)
    either (fail . ("YAML Emit Error: "++))
           (return . VStr) rv

toYaml :: Int -> Val -> Eval YamlNode
toYaml 0     _          = return $ YamlStr "<deep recursion>" -- fail? make this configurable?
toYaml _     VUndef     = return YamlNil
toYaml _     (VStr str) = return $ YamlStr (encodeUTF8 str)
toYaml (d+1) v@(VRef r) = do
    t  <- evalValType v
    ifValTypeIsa v "Hash" (hashToYaml d r) $ do
        v'      <- readRef r
        nodes   <- toYaml d v'
        ifValTypeIsa v "Array" (return nodes) $ case v' of
            VObject _   -> return nodes
            _           -> return (YamlMap Nothing [(YamlStr "<ref>", nodes)])
toYaml (d+1) (VList nodes) = do
    fmap YamlSeq $ mapM (toYaml d) nodes
toYaml (d+1) v@(VObject obj) = do
    -- ... dump the objAttrs
    -- XXX this needs fixing WRT demagicalized pairs:
    -- currently, this'll return Foo.new((attr => "value)), with the inner
    -- parens, which is, of course, wrong.
    hash    <- fromVal v :: Eval VHash
    attrs   <- toYaml d $ VRef (hashRef hash)
    return $ addTag (Just $ "!pugs:object/" ++ showType (objType obj)) attrs
    where
    addTag _   (YamlMap (Just x) _) = error ("can't add tag: already tagged with" ++ x)
    addTag tag (YamlMap _        m) = YamlMap tag m
toYaml _ v = (return . YamlStr . encodeUTF8 . pretty) v

hashToYaml :: Int -> VRef -> Eval YamlNode
hashToYaml d (MkRef (IHash hv)) = do
    h <- hash_fetch hv
    let assocs = Map.toList h
    yamlmap <- flip mapM assocs $ \(ka, va) -> do
        ka' <- toYaml d $ VStr ka
        va' <- toYaml d va
        return (ka', va')
    return $ YamlMap Nothing yamlmap
hashToYaml _ r = error ("unexpected node: " ++ show r)
   
{-
ifValTypeIsa v "Pair"
    (case v' of
        VList [ks, vs] -> do
            kStr <- toYaml d ks
            vStr <- toYaml d vs
            return $ YamlMap [(kStr, vStr)] -- assume a pair is a one-element hash
        _ -> toYaml d v'                    -- XXX: probably broken to blithingly ignore ref levels here
    )
    (ifValTypeIsa v "Hash"
        --fmap YamlMap $ mapM (\(k, v) -> do {k' <- toYaml k; v' <- toYaml v; return (k', v')}) Map.toList =<< hash_fetch v')
        (do 
            case r of
                MkRef (IHash hv) -> do
                    h <- hash_fetch hv
                    let assocs = Map.toList h
                    yamlmap <- mapM (\(k, v) -> do
                        k' <- toYaml d (VStr k)
                        v' <- toYaml d v
                        return (k', v')) assocs
                    return $ YamlMap yamlmap
                _ -> error ("can't process hash: " ++ show v') -- XXX
        )
        (do nodes <- toYaml d v'
            ifValTypeIsa v "Array"
                (return $ nodes)
                (return $ YamlMap [(YamlStr "<ref>", nodes)])) -- XXX
    )
-}
