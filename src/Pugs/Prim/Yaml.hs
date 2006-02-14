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
import Foreign.StablePtr
import Foreign.Ptr

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
            --let spec    = Map.fromList (vals :: [(String, Val)])
            --spec    <- liftSTM . newTVar . Map.map lazyScalar $ Map.fromList (vals :: [(String, Val)])
            spec'   <- liftSTM . newTVar $ Map.fromList (vals :: [(String, Val)])
            spec    <- liftSTM . readTVar $ spec'
            rule    <- fromVal =<< Map.lookup "rule" spec
            global  <- fromVal =<< Map.lookup "global" spec
            stringify <- fromVal =<< Map.lookup "stringify" spec
            adverbs <- Map.lookup "adverbs" spec
            return $ VRule MkRulePGE{rxRule=rule, rxGlobal=global, rxStringify=stringify, rxAdverbs=adverbs}
        Just x   -> error ("can't deserialize: " ++ x)

dumpYaml :: Int -> Val -> Eval Val
dumpYaml limit v = do
    obj  <- toYaml IntMap.empty v
    rv   <- liftIO (emitYaml obj)
    either (fail . ("YAML Emit Error: "++))
           (return . VStr . decodeUTF8) rv

strNode :: String -> YamlNode
strNode = mkNode . YamlStr

addressOf :: a -> IO Int
addressOf x = do
    ptr <- newStablePtr x
    return (castStablePtrToPtr ptr `minusPtr` (nullPtr :: Ptr ()))

toYaml :: IntMap YamlNode -> Val -> Eval YamlNode
toYaml _ VUndef       = return $ mkNode YamlNil
toYaml _ (VBool x)    = return $ boolToYaml x
toYaml _ (VStr str)   = return $ strNode (encodeUTF8 str)
toYaml seen v@(VRef r)   = do
    ptr <- liftIO $ addressOf r
    case IntMap.lookup ptr seen of
        Just node   -> return node
        Nothing     -> do
            rv <- ifValTypeIsa v "Hash" (hashToYaml seen r) $ do
                v'      <- readRef r
                nodes   <- toYaml seen v' -- XXX -- (IntMap.insert ptr rv seen) v'
                ifValTypeIsa v "Array" (return nodes) . return $ case v' of
                    VObject _   -> nodes
                    _           -> mkNode $ YamlMap [(strNode "<ref>", nodes)]
            return rv
toYaml seen (VList nodes) = do
    n <- mapM (toYaml seen) nodes
    return $ mkNode (YamlSeq n)
    -- fmap YamlSeq$ mapM toYaml nodes
toYaml seen v@(VObject obj) = do
    -- ... dump the objAttrs
    -- XXX this needs fixing WRT demagicalized pairs:
    -- currently, this'll return Foo.new((attr => "value)), with the inner
    -- parens, which is, of course, wrong.
    hash    <- fromVal v :: Eval VHash
    attrs   <- toYaml seen $ VRef (hashRef hash)
    return $ tagNode (Just $ "tag:pugs:object:" ++ showType (objType obj)) attrs
toYaml seen (VRule MkRulePGE{rxRule=rule, rxGlobal=global, rxStringify=stringify, rxAdverbs=adverbs}) =do
    adverbs' <- toYaml seen adverbs
    return . mkTagNode "tag:pugs:Rule" $ YamlMap
        [ (strNode "rule", strNode rule)
        , (strNode "global", boolToYaml global)
        , (strNode "stringify", boolToYaml stringify)
        , (strNode "adverbs", adverbs')
        ]
toYaml _ v = return $ strNode $ (encodeUTF8 . pretty) v

hashToYaml :: IntMap YamlNode -> VRef -> Eval YamlNode
hashToYaml seen (MkRef (IHash hv)) = do
    h <- hash_fetch hv
    let assocs = Map.toList h
    yamlmap <- forM assocs $ \(ka, va) -> do
        ka' <- toYaml seen $ VStr ka
        va' <- toYaml seen va
        return (ka', va')
    return $ mkNode (YamlMap yamlmap)
hashToYaml _ r = error ("unexpected node: " ++ show r)

boolToYaml :: VBool -> YamlNode
boolToYaml True  = strNode "true"
boolToYaml False = strNode "false"
