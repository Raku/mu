{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    PIL2 - The New Runcore.

>   And the Tree that was withered shall be renewed,
>   And he shall plant it in the high places,
>   And the City shall be blessed.
>   Sing all ye people!
-}

module PIL where
import PIL.Str (Str)
import qualified PIL.Str as Str
import PIL.Native
import PIL.Val
import PIL.Pad
import PIL.PIL
import PIL.Container
import PIL.Compile
import PIL.Monads
import PIL.Parser
import PIL.Internals
import System.IO
import Data.PackedString

-- Beginning of design of PIL2.

-- Goal: To make explicit the flow of types, after parsing and
-- before type erasure.

-- Goal: To unify Binding (Let) and Apply as the two verbs.

-- Goal: To introduce all symbols at beginning of Scope.

-- Goal: To facilitate separate compilation by exposing a link set
-- to the consuming module.  This starts from the file scope "main",
-- which we shall specify as a Code node that does not perform bindings.
-- A PPos annotator still works on any node whatsoever.

-- Goal: To allow runtime rebinding into different types, but check
-- the types at compile time and raise warnings nevertheless!

-- File is simply a toplevel code literal plus a set of external
-- visible linkset information.

main :: IO ()
main = do
    putStrLn "*** Welcome to PIL2 REPL, the Pugs Again Shell!"
    putStrLn "*** Please enter expressions, or :q to exit."
    fix $ \redo -> do
        putStr "pugs> "
        hFlush stdout
        src <- getLine
        hFlush stdout
        if (src == ":q") then return () else do
        banner "String -> Parse Exp"
        syn <- runM $ parse src
        print syn
        banner "Exp -> Compile PIL"
        pil <- runM $ compile syn
        print pil
        banner "PIL -> String"
        putStrLn $ pretty pil
        banner "Eval PIL"
        val <- runM $ interpret pil
        banner "Final Result"
        print val
        putStrLn ""
        redo
    where
    banner x = putStrLn ("\n*** " ++ x ++ " ***")

eval :: String -> IO Val
eval src = do
    syn <- runM (parse src)
    pil <- runM (compile syn)
    val <- runM (interpret pil)
    putStr "===> "
    print val
    return val

p1 :: String
p1 = "print 1"

-- Pretty is pure and needs no monad.
pretty :: PIL -> String
pretty (App f a) = concat [ pretty f, "(", pretty a, ")" ]
pretty (Lit (Item (Int x))) = show x
pretty (Var x) = (unSigil $ symSigil x) : (unName $ symName x)
pretty x = error $ "Can't prettyprint " ++ show x

-- Eval is pretty much all about side effects.
interpret :: PIL -> Eval Val
interpret (App f a) = do
    code <- interpret f
    arg  <- interpret a
    apply code arg
interpret (Lit x) = return x
interpret (Var x) | symSigil x == SigilCode = do
    -- XXX - hardcoded lookup for &code
    return . Item . Code $ MkCode (unName (symName x))
interpret (Var x) = fail $ "Unknown variable: " ++ show x

apply :: Val -> Val -> Eval Val
apply (Item (Code (MkCode "print"))) (Item v) = do
    Str.put stdout =<< stringify v
    return . Item . Bit $ True
apply (Item (Code (MkCode "say"))) (Item v) = do
    Str.put stdout =<< stringify v
    putStrLn ""
    return . Item . Bit $ True
apply x y = fail $ "Cannot apply: (" ++ show x ++ ").(" ++ show y ++ ")"

-- XXX Hack
stringify :: (Monad m) => Item -> m Str
stringify (Int x) = return $ Str.from x
stringify (Bit x) = return $ Str.from (fromEnum x)
stringify (Str x) = return x
stringify x = fail $ "Cannot stringify: " ++ show x


