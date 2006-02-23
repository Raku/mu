{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.YAML (genYAML, genParseYAML, genParseHsYAML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances
import Pugs.PIL1
import DrIFT.YAML
import qualified Data.Map as Map

genParseHsYAML :: Eval Val
genParseHsYAML = do
    glob        <- asks envGlobal
    MkPad pad   <- liftSTM $ readTVar glob
    pad'        <- fmap (MkPad . Map.fromAscList . catMaybes) . mapM checkPrim $ Map.toAscList pad
    -- munge the glob to filter out prim stuff in it
    main    <- asks envBody
    yaml    <- liftIO $ toYamlNode (pad', main)
    return (VStr $ show yaml)

genParseYAML :: Eval Val
genParseYAML = do
    glob        <- asks envGlobal
    MkPad pad   <- liftSTM $ readTVar glob
    pad'        <- fmap (MkPad . Map.fromAscList . catMaybes) . mapM checkPrim $ Map.toAscList pad
    -- munge the glob to filter out prim stuff in it
    main    <- asks envBody
    yaml    <- liftIO $ showYaml (pad', main)
    return (VStr yaml)

checkPrim :: (String, PadEntry) -> Eval (Maybe (String, PadEntry))
checkPrim ((':':'*':_), _) = return Nothing
checkPrim e@((_, MkEntry (_, tv))) = do
    rv <- isPrim tv
    return $ if rv then Nothing else Just e
checkPrim (key, MkEntryMulti xs) = do
    xs' <- filterM (fmap not . isPrim . snd) xs
    return $ if null xs' then Nothing else Just (key, MkEntryMulti xs')

isPrim :: TVar VRef -> Eval Bool
isPrim tv = do
    vref <- liftSTM $ readTVar tv
    case vref of
        MkRef (ICode cv)    -> fmap (isPrimVal . VCode) (code_fetch cv)
        MkRef (IScalar sv)  -> fmap isPrimVal (scalar_fetch sv)
        _                   -> return False
    where
    isPrimVal (VCode MkCode{ subBody = Prim _ }) = True
    isPrimVal _ = False

genYAML :: Eval Val
genYAML = do
    penv <- compile () :: Eval PIL_Environment
    yaml <- liftIO (showYaml penv)
    return (VStr yaml)
