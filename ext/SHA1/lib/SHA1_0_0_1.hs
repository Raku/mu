module SHA1_0_0_1 where
import AST
import SHA1

extern__ :: [String]
extern__ = ["sha1"]

extern__sha1 :: [Val] -> Eval Val
extern__sha1 [v] = do
    s <- fromVal v
    return . castV $ sha1 s
