import Mildew.AST
main = do
    input <- getContents
    print $ (read input :: Expr)
