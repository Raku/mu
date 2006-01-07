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
fromYaml MkYamlNode{el=YamlNil}       = return VUndef
fromYaml MkYamlNode{el=YamlStr str}   = return $ VStr (decodeUTF8 str)
fromYaml MkYamlNode{el=YamlSeq nodes} = do
    vals    <- mapM fromYaml nodes
    av      <- liftSTM $ newTVar $
        IntMap.fromAscList ([0..] `zip` map lazyScalar vals)
    return $ VRef (arrayRef av)
fromYaml MkYamlNode{el=YamlMap nodes} = do
    vals    <- forM nodes $ \(keyNode, valNode) -> do
        key <- fromVal =<< fromYaml keyNode
        val <- newScalar =<< fromYaml valNode
        return (key, val)
    hv      <- liftSTM $ (newTVar (Map.fromList vals) :: STM IHash)
    -- XXX: if YamlMap (Just "!perl/":type) nodes then mkObject etc.
    return $ VRef (hashRef hv)

dumpYaml :: Int -> Val -> Eval Val
dumpYaml limit v = let ?d = limit in do
    obj  <- toYaml v
    rv   <- liftIO (emitYaml obj)
    either (fail . ("YAML Emit Error: "++))
           (return . VStr) rv

toYaml :: (?d :: Int) => Val -> Eval YamlNode
toYaml _ | ?d == 0  = return $ emptyYamlNode{el = YamlStr "<deep recursion>"} -- fail? make this configurable?
toYaml VUndef       = return emptyYamlNode
toYaml (VStr str)   = return $ emptyYamlNode{el = YamlStr (encodeUTF8 str)}
toYaml v@(VRef r)   = let ?d = pred ?d in do
    t  <- evalValType v
    ifValTypeIsa v "Hash" (hashToYaml r) $ do
        v'      <- readRef r
        nodes   <- toYaml v'
        ifValTypeIsa v "Array" (return nodes) $ case v' of
            VObject _   -> return nodes
            _           -> return emptyYamlNode{el = YamlMap [(emptyYamlNode{el=YamlStr "<ref>"}, nodes)]}
toYaml (VList nodes) = let ?d = pred ?d in do
    n <- mapM toYaml nodes
    return $ emptyYamlNode{el=YamlSeq n} -- golfme!
    -- fmap YamlSeq$ mapM toYaml nodes
toYaml v@(VObject obj) = let ?d = pred ?d in do
    -- ... dump the objAttrs
    -- XXX this needs fixing WRT demagicalized pairs:
    -- currently, this'll return Foo.new((attr => "value)), with the inner
    -- parens, which is, of course, wrong.
    hash    <- fromVal v :: Eval VHash
    attrs   <- toYaml $ VRef (hashRef hash)
    return $ tagNode (Just $ "tag:pugs:object:" ++ showType (objType obj)) attrs
toYaml v = return $ emptyYamlNode{el=YamlStr p}
    where p = (encodeUTF8 . pretty) v


hashToYaml :: (?d :: Int) => VRef -> Eval YamlNode
hashToYaml (MkRef (IHash hv)) = do
    h <- hash_fetch hv
    let assocs = Map.toList h
    yamlmap <- flip mapM assocs $ \(ka, va) -> do
        ka' <- toYaml $ VStr ka
        va' <- toYaml va
        return (ka', va')
    return $ emptyYamlNode{el=YamlMap yamlmap}
hashToYaml r = error ("unexpected node: " ++ show r)

