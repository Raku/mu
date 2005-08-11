{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Compile where
import qualified PIL.Exp as Exp
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
-- XXX kluge! doesn't do anything!
compile :: Exp.Exp -> Compile PIL
compile (Exp.App x y) = do
    x' <- compile x
    y' <- compile y
    return $ App x' y'
compile (Exp.Var x)   = return $ Var x
compile (Exp.Lit x)   = return $ Lit x

