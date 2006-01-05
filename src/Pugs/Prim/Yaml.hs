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

dumpYaml :: Int -> Val -> Eval Val
dumpYaml limit v = do
    obj  <- toYaml limit =<< fromVal v
    rv   <- liftIO (emitYaml obj)
    case rv of
        Left err  -> fail $ "YAML Emit Error: " ++ err
        Right str -> return $ VStr str

toYaml :: Int -> Val -> Eval YamlNode
toYaml 0 _ = return $ YamlStr "<deep recursion>" -- fail? make this configurable?
toYaml _ VUndef = return YamlNil
--toYaml (VNum num) = return $ YamlStr -- better handled by pretty?
toYaml _ (VStr str) = return $ YamlStr (encodeUTF8 str)
toYaml (d+1) (VList nodes) = do
    fmap YamlSeq $ mapM (toYaml d) nodes
toYaml (d+1) v@(VRef r) = do  -- stolen from Pugs.Prim prettyVal. Can these be refactored together?
    v'  <- readRef r
    ifValTypeIsa v "Pair"
        (case v' of
            VList [ks, vs] -> do
                kStr <- toYaml d ks
                vStr <- toYaml d vs
                return $ YamlMap [(kStr, vStr)] -- assume a pair is a one-element hash
            _ -> toYaml d v'                    -- XXX: probably broken to blithingly ignore ref levels here
        )
        (do nodes <- toYaml d v'
            ifValTypeIsa v "Array"
                (return $ nodes)
                (ifValTypeIsa v "Hash"
                    --(return $ YamlMap('{':(init (tail str))) ++ "}")
                    (return nodes)
                    (return $ YamlMap [(YamlStr "<ref>", nodes)])) -- XXX
        )
toYaml _ v = return $ YamlStr $ encodeUTF8 $ pretty v


    
