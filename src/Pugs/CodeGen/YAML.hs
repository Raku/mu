{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.YAML (genYAML, genParseYAML, genParseHsYAML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import DrIFT.YAML

genParseHsYAML, genParseYAML :: FilePath -> Eval Val
genParseHsYAML file = doGenParseYAML file (fmap show . toYamlNode)
genParseYAML   file = doGenParseYAML file showYamlCompressed

doGenParseYAML :: FilePath -> (CompUnit -> IO String) -> Eval Val
doGenParseYAML file f = do
    pad  <- filterPrim =<< asks envGlobal
    main <- asks envBody
    yaml <- io $ f $ mkCompUnit file pad main
    return $ VStr yaml

-- XXX: do something useful with the filename arg
genYAML :: FilePath -> Eval Val
genYAML _ = do
    penv <- compile () :: Eval PIL_Environment
    yaml <- io (showYamlCompressed penv)
    return $ VStr yaml

