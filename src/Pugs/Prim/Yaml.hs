{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

module Pugs.Prim.Yaml (
  evalYaml, dumpYaml, addressOf,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Pretty
import Pugs.Types
import Data.Yaml.Syck
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.ByteString as Str
import DrIFT.YAML

evalYaml :: Val -> Eval Val
evalYaml cv = do
    str     <- fromVal cv
    node    <- guardIO (parseYaml $ encodeUTF8 str)
    fromYaml node

fromYaml :: YamlNode -> Eval Val
fromYaml MkYamlNode{nodeElem=YamlNil}       = return VUndef
fromYaml MkYamlNode{nodeElem=YamlStr str}   = return $ VStr $ decodeUTF8 $ unpackBuf str
fromYaml MkYamlNode{nodeElem=YamlSeq nodes} = do
    vals    <- mapM fromYaml nodes
    av      <- liftSTM $ newTVar $
        IntMap.fromAscList ([0..] `zip` map lazyScalar vals)
    return $ VRef (arrayRef av)
fromYaml MkYamlNode{nodeElem=YamlMap nodes, nodeTag=tag} = do
    case tag of
        Nothing  -> do
            vals    <- forM nodes $ \(keyNode, valNode) -> do
                key <- fromVal =<< fromYaml keyNode
                val <- newScalar =<< fromYaml valNode
                return (key, val)
            hv      <- liftSTM $ (newTVar (Map.fromList vals) :: STM IHash)
            return $ VRef (hashRef hv)
        Just s | (pre, post) <- Str.splitAt 16 s   -- 16 == length "tag:pugs:Object:"
               , pre == packBuf "tag:pugs:Object:" -> do
            let typ = unpackBuf post
            vals    <- forM nodes $ \(keyNode, valNode) -> do
                key <- fromVal =<< fromYaml keyNode
                val <- fromYaml valNode
                return (key, val)
            return . VObject =<< createObject (mkType typ) vals
        Just s | s == packBuf "tag:pugs:Rule" -> do
            vals    <- forM nodes $ \(keyNode, valNode) -> do
                key <- fromVal =<< fromYaml keyNode
                val <- fromYaml valNode
                return (key, val)
            --let spec    = Map.fromList (vals :: [(String, Val)])
            --spec    <- liftSTM . newTVar . Map.map lazyScalar $ Map.fromList (vals :: [(String, Val)])
            spec'   <- liftSTM . newTVar $ Map.fromList (vals :: [(String, Val)])
            spec    <- liftSTM . readTVar $ spec'
            rule    <- fromVal =<< Map.lookup "rule" spec
            global  <- fromVal =<< Map.lookup "global" spec
            stringify <- fromVal =<< Map.lookup "stringify" spec
            adverbs <- Map.lookup "adverbs" spec
            return $ VRule MkRulePGE{rxRule=rule, rxGlobal=global, rxStringify=stringify, rxAdverbs=adverbs}
        Just x   -> error ("can't deserialize: " ++ unpackBuf x)

dumpYaml :: Val -> Eval Val
dumpYaml v = do
    let ?seen = IntSet.empty
    obj     <- toYaml v
    rv      <- guardIO . emitYaml $ obj
    (return . VStr . decodeUTF8) rv

strNode :: String -> YamlNode
strNode = mkNode . YamlStr . packBuf

{-
addressOf :: a -> IO Int
addressOf x = do
    ptr <- newStablePtr x
    return (castStablePtrToPtr ptr `minusPtr` (nullPtr :: Ptr ()))
-}

toYaml :: (?seen :: IntSet.IntSet) => Val -> Eval YamlNode
toYaml VUndef       = return $ mkNode YamlNil
toYaml (VBool x)    = return $ boolToYaml x
toYaml (VStr str)   = return $ strNode (encodeUTF8 str)
toYaml v@(VRef r)   = do
    ptr <- liftIO $ addressOf r
    if IntSet.member ptr ?seen then return nilNode{ nodeAnchor = MkYamlReference ptr } else do
        let ?seen = IntSet.insert ptr ?seen
        node <- ifValTypeIsa v "Hash" (hashToYaml r) $ do
            v'      <- readRef r
            nodes   <- toYaml v'
            ifValTypeIsa v "Array" (return nodes) $ case v' of
                VObject _   -> return nodes
                _           -> liftIO $ toYamlNode r
        return node{ nodeAnchor = MkYamlAnchor ptr }
toYaml (VList nodes) = do
    n <- mapM toYaml nodes
    return $ mkNode (YamlSeq n)
    -- fmap YamlSeq$ mapM toYaml nodes
toYaml v@(VObject obj) = do
    -- ... dump the objAttrs
    -- XXX this needs fixing WRT demagicalized pairs:
    -- currently, this'll return Foo.new((attr => "value)), with the inner
    -- parens, which is, of course, wrong.
    hash    <- fromVal v :: Eval VHash
    attrs   <- toYaml $ VRef (hashRef hash)
    return $ tagNode (Just $ packBuf $ "tag:pugs:Object:" ++ showType (objType obj)) attrs
toYaml (VRule MkRulePGE{rxRule=rule, rxGlobal=global, rxStringify=stringify, rxAdverbs=adverbs}) = do
    adverbs' <- toYaml adverbs
    return . mkTagNode "tag:pugs:Rule" $ YamlMap
        [ (strNode "rule", strNode rule)
        , (strNode "global", boolToYaml global)
        , (strNode "stringify", boolToYaml stringify)
        , (strNode "adverbs", adverbs')
        ]
toYaml v = return $ strNode $ (encodeUTF8 . pretty) v

hashToYaml :: (?seen :: IntSet.IntSet) => VRef -> Eval YamlNode
hashToYaml (MkRef (IHash hv)) = do
    h <- hash_fetch hv
    let assocs = Map.toList h
    yamlmap <- forM assocs $ \(ka, va) -> do
        ka' <- toYaml $ VStr ka
        va' <- toYaml va
        return (ka', va')
    return $ mkNode (YamlMap yamlmap)
hashToYaml r = error ("unexpected node: " ++ show r)

boolToYaml :: VBool -> YamlNode
boolToYaml True  = strNode "true"
boolToYaml False = strNode "false"
