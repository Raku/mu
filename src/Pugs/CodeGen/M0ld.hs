{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.M0ld (genM0ld) where
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1

-- XXX: do something useful with the filename arg
genM0ld :: FilePath -> Eval Val
genM0ld filepath = do
    penv <- compile () :: Eval PIL_Environment
    return $ VStr $ filepath++"\n"
