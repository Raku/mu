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
fromYaml MkYamlNode{el=YamlMap nodes,tag=tag} = do
    case tag of
        Nothing  -> do
            vals    <- forM nodes $ \(keyNode, valNode) -> do
                key <- fromVal =<< fromYaml keyNode
                val <- newScalar =<< fromYaml valNode
                return (key, val)
            hv      <- liftSTM $ (newTVar (Map.fromList vals) :: STM IHash)
            return $ VRef (hashRef hv)
        Just ('p':'u':'g':'s':'/':'o':'b':'j':'e':'c':'t':':':typ) -> do
            vals    <- forM nodes $ \(keyNode, valNode) -> do
                key <- fromVal =<< fromYaml keyNode
                val <- fromYaml valNode
                return (key, val)
            return . VObject =<< createObject (mkType typ) vals
        Just "pugs/Rule" -> do
            vals    <- forM nodes $ \(keyNode, valNode) -> do
                key <- fromVal =<< fromYaml keyNode
                val <- fromYaml valNode
                return (key, val)
            let spec    = Map.fromList (vals :: [(String, Val)])
            rule    <- fromVal =<< Map.lookup "rule" spec
            global  <- fromVal =<< Map.lookup "global" spec
            stringify <- fromVal =<< Map.lookup "stringify" spec
            adverbs <- Map.lookup "adverbs" spec
            return $ VRule MkRulePGE{rxRule=rule, rxGlobal=global, rxStringify=stringify, rxAdverbs=adverbs}
        Just x   -> error ("can't deserialize: " ++ x)

dumpYaml :: Int -> Val -> Eval Val
dumpYaml limit v = let ?d = limit in do
    obj  <- toYaml v
    rv   <- liftIO (emitYaml obj)
    either (fail . ("YAML Emit Error: "++))
           (return . VStr) rv

strNode :: String -> YamlNode
strNode str = emptyYamlNode{el = YamlStr str }

toYaml :: (?d :: Int) => Val -> Eval YamlNode
toYaml _ | ?d == 0  = return $ strNode "<deep recursion>" -- fail? make this configurable?
toYaml VUndef       = return emptyYamlNode
toYaml (VBool x)    = return $ boolToYaml x
toYaml (VStr str)   = return $ strNode (encodeUTF8 str)
toYaml v@(VRef r)   = let ?d = pred ?d in do
    t  <- evalValType v
    ifValTypeIsa v "Hash" (hashToYaml r) $ do
        v'      <- readRef r
        nodes   <- toYaml v'
        ifValTypeIsa v "Array" (return nodes) $ case v' of
            VObject _   -> return nodes
            _           -> return emptyYamlNode{el = YamlMap [(strNode "<ref>", nodes)]}
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
toYaml (VRule MkRulePGE{rxRule=rule, rxGlobal=global, rxStringify=stringify, rxAdverbs=adverbs}) = let ?d = pred ?d in do
    adverbs' <- toYaml adverbs
    return emptyYamlNode{el = YamlMap
                            [ (strNode "rule", strNode rule)
                            , (strNode "global", boolToYaml global)
                            , (strNode "stringify", boolToYaml stringify)
                            , (strNode "adverbs", adverbs')
                            ] , tag = Just "tag:pugs:Rule"}
toYaml v = return $ strNode $ (encodeUTF8 . pretty) v


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

boolToYaml :: VBool -> YamlNode
boolToYaml True  = strNode "true"
boolToYaml False = strNode "false"
