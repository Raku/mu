import Mildew.AST 
import Mildew.AST2IR
import Compiler.Hoopl
main = do
    input <- getContents
    let (Block stmts regs) = (read input :: Expr)
    putStrLn $ showGraph show (runSimpleUniqueMonad $ runWithFuel 0 $ run $ open stmts)
