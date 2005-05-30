{-# OPTIONS_GHC -fglasgow-exts #-}

{- | An attempt at implementing the approach dons outlined.  Probably
   kinda redundant with Pugs.Compile.Haskell, but that doesn't work
   yet and maybe this will work.
 -}


module Pugs.Compile.Pugs2 (genPugs2) where
import Pugs.Internals
import Pugs.AST
-- import Pugs.Run
-- import Pugs.Prim

import Language.Haskell.Syntax
import Language.Haskell.Pretty

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

class Show pugs => Compile pugs hs | pugs -> hs where
    compile :: pugs -> hs

genPugs2, gen :: Eval Val
genPugs2 = gen
gen = do exp     <- asks envBody
         glob    <- askGlobal
         let mod = (HsModule 
                    noLoc
                    (Module "MainCC")
                    Nothing
                    (map (\modname -> HsImportDecl 
                          { importLoc       = noLoc,
                            importModule    = Module modname,
                            importQualified = False,
                            importAs        = Nothing,
                            importSpecs     = Nothing })
                     ["Pugs.Run","Pugs.AST","Pugs.Types","Pugs.Internals"])
                    [HsPatBind 
                     noLoc
                     (HsPVar (HsIdent "mainCC"))
                     (HsUnGuardedRhs (HsDo []))
                     []
                    ]
                   )
         return $ VStr $ prettyPrint mod
                     
                          

                           

         

