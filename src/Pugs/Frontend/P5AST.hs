{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Frontend.P5AST where
import Pugs.AST
import Pugs.Run
import Data.Typeable

infixl 4 <~~
infixl 4 <<<

data P5AST
    = P5AST [P5AST]
    | Bare String
    | Items [P5AST]
    | OpNextState [P5AST]
    | OpScope [P5AST]
    | OpPrint [P5AST]
    | OpConst [P5AST]
    | OpCondExpr [P5AST]
    | OpLeave [P5AST]
    | OpNextstate [P5AST]
    | OpSassign [P5AST]
    | OpSeq [P5AST]
    | OpSne [P5AST]
    | OpStringify [P5AST]
    deriving (Show, Eq, Read, Typeable)

p5pil :: P5AST -> Exp
p5pil (P5AST exps) = p5pil <~~ exps
p5pil (OpLeave exps) = p5pil <~~ exps
p5pil (Items exps) = p5pil <~~ exps
p5pil (Bare _) = Noop -- Val (VStr str)
p5pil (OpPrint exps) = fun "print" (p5print <<< exps)
p5pil _ = Noop

fun :: String -> [Exp] -> Exp
fun str args = App (Var ('&':str)) Nothing args

p5print :: P5AST -> [Exp]
p5print (Bare _) = []
p5print (Items xs) = concatMap p5print xs
p5print (OpConst exps) = [Val (VStr $ read (concat (p5str <<< exps)))]
p5print x = error $ show x

p5str :: P5AST -> [String]
p5str (Bare x) = [x]
p5str (Items xs) = p5str <<< xs
p5str (OpStringify xs) = p5str <<< xs
p5str (OpConst xs) = p5const <<< xs
p5str x = error $ show x

p5const :: P5AST -> [String]
p5const (Bare x) = [x]
p5const x = error $ show x

(<<<) :: (P5AST -> [a]) -> [P5AST] -> [a]
f <<< exps = concatMap f exps

flatten :: P5AST -> [P5AST]
flatten (Bare _) = []
flatten (Items exps) = exps
flatten x = [x]

(<~~) :: (P5AST -> Exp) -> [P5AST] -> Exp
f <~~ exps = foldr mergeStmts Noop (map f exps)

{-
import Pugs.Run
import Pugs.AST.Internals
import Data.Map

test :: P5AST -> IO Val
test ast = runAST (MkPad empty) (p5pil ast)

ast :: P5AST
ast = P5AST
    [ OpLeave [Items [ ] , Items [ Bare "#!./perl\n\n# $RCSfile: if.t,v $$Revision: 4.1 $$Date: 92/08/07 18:27:03 $\n\n" ] , Items [ OpNextstate [] ] , Items [ Bare "print" , OpPrint
        [Items [ ] , Items [ Bare " " , OpConst [Bare "\"" , OpStringify [Bare "" , OpConst [Bare "1..2\\n" ] , Bare "" ] , Bare "\"" ] ] ]
    ] , Items [ Bare ";" , OpNextstate [] , Bare "\n" ] , Items [ OpSassign [Items [ Bare "\n# first test to see if we can run the tests.\n\n" , Bare "$x" ] , Bare " " , Bare "=" , Items [ Bare " " , OpConst [Bare "'test'" ] ] ] ] , Items [ Bare ";" , OpNextstate [] , Bare "\n" ] , Items [ OpCondExpr [Items [ Bare "if" , Bare " " , Bare "(" , OpSeq [Items [ Bare "$x" ] , Bare " " , Bare "eq" , Items [ Bare " " , Bare "$x" ] ] , Bare ")" ] , Items [ Bare " " , Bare "{" , OpScope [Items [ ] , Items [ Bare " " , Bare "print" , OpPrint [Items [ ] , Items [ Bare " " , OpConst [Bare "\"" , OpStringify [Bare "" , OpConst [Bare "ok 1\\n" ] , Bare "" ] , Bare "\"" ] ] ] ] ] , Bare ";" , Bare " " , Bare "}" ] , Items [ Bare " " , Bare "else" , Bare " " , Bare "{" , OpLeave [Items [ ] , Items [ OpNextstate [] ] , Items [ Bare " " , Bare "print" , OpPrint [Items [ ] , Items [ Bare " " , OpConst [Bare "\"" , OpStringify [Bare "" , OpConst [Bare "not ok 1\\n" ] , Bare "" ] , Bare "\"" ] ] ] ] ] , Bare ";" , Bare "}" , Bare "\n" ] ] ] , Items [ OpNextstate [] ] , Items [ OpCondExpr [Items [ Bare "if" , Bare " " , Bare "(" , OpSne [Items [ Bare "$x" ] , Bare " " , Bare "ne" , Items [ Bare " " , Bare "$x" ] ] , Bare ")" ] , Items [ Bare " " , Bare "{" , OpScope [Items [ ] , Items [ Bare " " , Bare "print" , OpPrint [Items [ ] , Items [ Bare " " , OpConst [Bare "\"" , OpStringify [Bare "" , OpConst [Bare "not ok 2\\n" ] , Bare "" ] , Bare "\"" ] ] ] ] ] , Bare ";" , Bare " " , Bare "}" ] , Items [ Bare " " , Bare "else" , Bare " " , Bare "{" , OpLeave [Items [ ] , Items [ OpNextstate [] ] , Items [ Bare " " , Bare "print" , OpPrint [Items [ ] , Items [ Bare " " , OpConst [Bare "\"" , OpStringify [Bare "" , OpConst [Bare "ok 2\\n" ] , Bare "" ] , Bare "\"" ] ] ] ] ] , Bare ";" , Bare "}" , Bare "\n" ] ] ] ]
    ] 

-}
