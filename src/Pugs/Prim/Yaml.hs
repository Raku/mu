{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.Prim.Yaml (
  evalYaml, dumpYaml
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Pretty
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
fromYaml (YamlStr str) = return $ VStr (decodeUTF8 str)
fromYaml (YamlSeq nodes) = do
    vals    <- mapM fromYaml nodes
    av      <- liftSTM $ newTVar $
        IntMap.fromAscList ([0..] `zip` map lazyScalar vals)
    return $ VRef (arrayRef av)
fromYaml (YamlMap nodes) = do
    vals    <- forM nodes $ \(keyNode, valNode) -> do
        key <- fromVal =<< fromYaml keyNode
        val <- newScalar =<< fromYaml valNode
        return (key, val)
    hv      <- liftSTM $ (newTVar (Map.fromList vals) :: STM IHash)
    return $ VRef (hashRef hv)

dumpYaml :: Val -> Eval Val
dumpYaml v = do
    obj  <- toYaml =<< fromVal v
    rv   <- liftIO (emitYaml obj)
    case rv of
        Left err  -> fail $ "YAML Emit Error: " ++ err
        Right str -> return $ VStr str

toYaml :: Val -> Eval YamlNode
toYaml VUndef = return YamlNil
--toYaml (VNum num) = return $ YamlStr -- better handled by pretty
toYaml (VStr str) = return $ YamlStr (encodeUTF8 str)
toYaml (VList nodes) = do
    fmap YamlSeq $ mapM toYaml nodes
toYaml x = return $ YamlStr $ encodeUTF8 $ pretty x
--toYaml (VHash hash) = do
--    fmap YamlMap $ Map.toList hash


    
