{-# OPTIONS -fglasgow-exts #-}

{-
    Parrot IMC implementation.

    All that is gold does not glitter,
    Not all those who wander are lost;
    The old that is strong does not wither,
    Deep roots are not reached by the frost.
-}

module IMC (
    module IMC.Compile,
    module Internals.TH,
    yow,
) where
import IMC.AST
import IMC.Compile
import Internals.TH
import Language.Haskell.TH.Ppr
-- import IMC.Lexer


runFoo = undefined
yow :: String -> IO ()
yow str = do
    prog' <- runFoo
    $( imcX prog' ) -- how to get str into prog?
