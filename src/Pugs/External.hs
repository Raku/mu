{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-
    External call utilities.

    To the Sea, to the Sea! The white gulls are crying,
    The wind is blowing, and the white foam is flying.
    West, west away, the round sun is falling.
    Grey ship, grey ship, do you hear them calling?
-}

module Pugs.External where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.External.Haskell (externalizeHaskell, loadHaskell)

externalize :: String -> Exp -> IO String
externalize mod (Statements stmts) = externExternalize backend mod code
    where
    (backend, code)
        | null things   = error "no inline found"
        | [_] <- things = head things
        | otherwise     = error "multiple inline found"
    things = [ (backend, code)
             | (Syn "inline" [Val (VStr backend), Val (VStr code)], _) <- stmts
             ]
externalize _ _ = error "not statements"

externExternalize "Haskell" = externalizeHaskell
externExternalize backend   = error $ "Unrecognized inline backend: " ++ backend

externLoad "Haskell" = loadHaskell
externLoad backend   = error $ "Unrecognized inline backend: " ++ backend

externRequire lang name = do
    glob    <- asks envGlobal
    liftIO $ do
        bindings    <- externLoad lang name
        syms        <- readIORef glob
        writeIORef glob (map gensym bindings ++ syms)
    where
    gensym (name, fun) = MkSym ('&':name) . codeRef $ MkCode
        { isMulti       = True
        , subName       = ('&':name)
        , subPad        = []
        , subType       = SubPrim
        , subAssoc      = "pre"
        , subParams     = [buildParam "List" "" "*@?1" (Val VUndef)]
        , subBindings   = []
        , subReturns    = anyType
        , subSlurpLimit = []
        , subFun        = (Prim fun)
        }

