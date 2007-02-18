{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.YAML (genYAML, genParseYAML, genParseHsYAML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import DrIFT.YAML

genParseHsYAML, genParseYAML :: Eval Val
genParseHsYAML = doGenParseYAML (fmap show . toYamlNode)
genParseYAML   = doGenParseYAML showYaml

doGenParseYAML :: (CompUnit -> IO String) -> Eval Val
doGenParseYAML f = do
    pad  <- filterPrim =<< asks envGlobal
    main <- asks envBody
    yaml <- liftIO $ f $ mkCompUnit "<unused>" pad main
    return $ VStr yaml

genYAML :: Eval Val
genYAML = do
    penv <- compile () :: Eval PIL_Environment
    yaml <- liftIO (showYaml penv)
    return $ VStr yaml
