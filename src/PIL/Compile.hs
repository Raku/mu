{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Compile where
import PIL.Exp
import PIL.PIL
import PIL.Monads
import Control.Monad.Error

-- prelude -- initial (lexical!) environment.
type Compile = Either String

instance MonadRunM (Either String) IO where
    runM x = case x of
        Left err -> fail err
        Right x' -> return x'

-- Compilation could be pure... we'll see.
compile :: Exp -> Compile PIL
compile = undefined

