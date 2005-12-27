{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.ToJavaScript
    (JSCompiler, JSFun, newJSCompiler, compileJS, dumpBodies
    ) where

import Control.Monad.State
import Data.IORef
import Data.List(intersperse)
import GHC.Prim(unsafeCoerce#)
import qualified Data.IntMap as I
import System.Mem.StableName
import Text.Parser.PArrow.MD(MD(..))

-- | JSFun encapsulates a reference to a JavaScript function.
newtype JSFun = JSFun Int
-- | JSCompiler encapsulates the state of the JavaScript compiler.
newtype JSCompiler = JSC (IORef JSCompState)
type JSComp a = StateT JSCompState IO a
data INVALID = INVALID
type SN = StableName (MD INVALID INVALID)
data JSCompState = JSCompState { bodies :: I.IntMap String,
                                 defs :: I.IntMap [(SN,JSFun)],
                                 count :: Int,
                                 prefix :: String
                               }
-- | Create a new JavaScript compiler using the suplied string as prefix.
-- Returns the compiler and a function for showing function references.
newJSCompiler :: String -> IO (JSCompiler, JSFun -> String)
newJSCompiler pref = do c <- newIORef (JSCompState I.empty I.empty 1 pref)
                        return (JSC c, \(JSFun i) -> pref++show i)
-- | Compile a parser into JavaScript. Returns a reference to the top-level
-- Parsing function. The generated javascript function expects a String and
-- a starting index for parsing. The result will be either the index of the
-- rightmost character matched or -1 if the parser failed.
compileJS :: JSCompiler -> MD i o -> IO JSFun
compileJS (JSC jsc) p = do s <- readIORef jsc
                           (v,s') <- runStateT (toJavaScriptJSFun p) s
                           writeIORef jsc s'
                           return v

-- | Dump all bodies of generated JavaScript functions.
dumpBodies :: JSCompiler -> IO [(JSFun,String)]
dumpBodies (JSC jsc) = readIORef jsc >>= return . map (\(a,b) -> (JSFun a,b)) . I.toList . bodies

-- Implementation

toJavaScriptSingle :: JSFun -> MD i o -> JSComp String
toJavaScriptSingle n (MEqual ch) = cfun n ["{return (s.charAt(i)==",show ch,")?i+1:-1}"]
toJavaScriptSingle n (MEmpty)    = cfun n ["{return i}"]
toJavaScriptSingle n (MPure _ _) = toJavaScriptSingle n MEmpty
toJavaScriptSingle n (MSeq a b)  = do af <- toJavaScriptFunRef a
                                      bf <- toJavaScriptFunRef b
                                      cfun n ["{return ",bf,"(s,",af,"(s,i))}"]
toJavaScriptSingle n (MCSet cs)  = cfun n ["{return (s.charAt(i).search(/^",show cs,"/)!=-1?i+1:-1)}"]
toJavaScriptSingle n (MStar p)   = do pf <- toJavaScriptFunRef p
                                      cfun n ["{for(j=i;j>=0;){i=j;j=",pf,"(s,j)}return i}"]
toJavaScriptSingle n (MChoice cs)= do rfs <- mapM toJavaScriptFunRef cs
                                      cfun n ["{a=[",concat (intersperse "," rfs),"];",
                                              "for(j=0;j<a.length;j++){",
                                              "r=a[j](s,i);if(r>=0){return r}}",
                                              "return -1}"]
toJavaScriptSingle n (MJoin a b) = do af <- toJavaScriptFunRef a
                                      bf <- toJavaScriptFunRef b
                                      cfun n ["{return ",bf,"(s,",af,"(s,i))}"]
toJavaScriptSingle n (MNot p)    = do pf <- toJavaScriptFunRef p
                                      cfun n ["{return (",pf,"(s,i)>=0?-1:i)}"]


cfun :: JSFun -> [String] -> JSComp String
cfun n lst = do cn <- jsFunName n
                return $ concat ("function ":cn:"(s,i) ":lst)

jsFunName (JSFun v) = gets prefix >>= \e -> return (e ++ show v)

toJavaScriptFunRef :: MD i o -> JSComp String
toJavaScriptFunRef p = toJavaScriptJSFun p >>= jsFunName

toJavaScriptJSFun :: MD i o -> JSComp JSFun
toJavaScriptJSFun p = do
  cache <- gets defs
  psn <- makeSN p
  case I.lookup (hashStableName psn) cache >>= lookup psn of
   Nothing -> insertFunRef psn p
   Just x  -> return x

insertFunRef :: SN -> MD i o -> JSComp JSFun
insertFunRef psn p = do
  ci <- gets count
  modify (\s -> s { defs = I.insertWith (++) (hashStableName psn) [(psn,JSFun ci)] (defs s), count = ci+1 })
  pval <- toJavaScriptSingle (JSFun ci) p
  modify (\s -> s { bodies = I.insert ci pval (bodies s) })
  return (JSFun ci)

makeSN :: MD i o -> JSComp SN
makeSN md = liftIO (unsafeCoerce# (makeStableName md))
