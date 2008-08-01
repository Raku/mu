{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.Binary (genParseBinary) where
import Pugs.Internals
import Pugs.AST
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as L

genParseBinary :: FilePath -> Eval Val
genParseBinary file = doGenParseBinary file

doGenParseBinary :: FilePath -> Eval Val
doGenParseBinary file = do
    pad  <- filterPrim =<< asks envGlobal
    main <- asks envBody
    return $ VStr (L.unpack $! encode (mkCompUnit file pad main))
