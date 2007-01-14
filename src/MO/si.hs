{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- To compile make pugs as usual and use the following command inside src/ directory:
-- ghc -package-conf ../third-party/installed/packages.conf -lpcre -ipcre --make MO/si.hs

import MO.Run
import MO.Compile
import MO.Compile.Class
import MO.Util
import Data.Typeable
import Pugs.Val
import Pugs.Pretty
import Pugs.Internals

import GHC.Unicode (toUpper, toLower)


-- A cast-map
class (Typeable a, Ord a, Typeable1 m, Monad m) => Boxable m a | a -> m where
    classOf :: a -> MI m
    fromObj :: Invocant m -> m a

-- Helpers for Str type
capitalize = mapEachWord capitalizeWord
  where
    mapEachWord _ [] = []
    mapEachWord f str@(c:cs)
        | isSpace c = c:(mapEachWord f cs)
        | otherwise = f word ++ mapEachWord f rest
          where (word,rest) = break isSpace str
    capitalizeWord []     = []
    capitalizeWord (c:cs) = toUpper c:(map toLower cs)

toQuoteMeta :: Char -> String
toQuoteMeta c =
   if not(isLatin1 c) -- Ignore Unicode characters beyond the 256-th
      || isAsciiUpper c || isAsciiLower c || isDigit c || c == '_'
      then [ c ]
      else [ '\\', c ]

-- XXX - Once MI for native types is made generally "is open" this must be adjusted as well.
instance Boxable IO String where
    classOf _ = mkBoxClass "Str"
        [ "reverse"    ... (reverse :: String -> String)
        , "chop"       ... (\s -> if null s then s else init s)
        , "split"      ... words
        , "lc"         ... map toLower
        , "lcfirst"    ... (\s -> if null s then s else (toLower (head s)) : (tail s))
        , "uc"         ... map toUpper
        , "ucfirst"    ... (\s -> if null s then s else (toUpper (head s)) : (tail s))
        , "capitalize" ... capitalize
        , "quotemeta"  ... (concat . map toQuoteMeta)
        , "chars"      ... length
        --, "graphs"     ... undefined
        --, "codes"      ... undefined
        --, "bytes"      ... undefined
        --, "index"      ... undefined
        --, "rindex"     ... undefined
        --, "substr"     ... undefined
        , "test"       !!! (print :: String -> IO ())   -- just to test (!!!) function
        ]
    fromObj (MkInvocant x _) = undefined

instance Boxable IO Int where
    classOf _ = mkBoxClass "Int"
        [ "chr"     ... ((:[]) . chr)
        ]
    fromObj (MkInvocant x _) = undefined

instance Boxable IO a => Boxable IO [a] where
    classOf _ = mkBoxClass "List"
        [ "elems"   ... (length :: [String] -> Int)
        ]
    fromObj (MkInvocant x _) = undefined

{-

-- Doesn't work, don't know exactly why not...

instance (Typeable a, Ord a, Num a) => Boxable IO a where
    classOf _ = mkBoxClass "Num"
        [ "abs"      ... abs
        -- , "floor"    ... undefined
        -- , "ceiling"  ... undefined
        -- , "round"    ... undefined
        -- , "truncate" ... undefined 
        -- , "exp"      ... undefined 
        -- , "log"      ... undefined 
        -- , "log10"    ... undefined 
        -- , "log2"     ... undefined -- :-)
        -- , "rand"     ... undefined 
        -- , "sign"     ... undefined 
        -- , "srand"    ... undefined 
        -- , "sqrt"     ... undefined 
        ]
    fromObj (MkInvocant x _) = undefined
-}

instance Boxable IO Char where
    fromObj (MkInvocant x _) = undefined

instance Boxable IO () where
    fromObj (MkInvocant x _) = undefined


instance Boxable IO Socket where
    classOf _ = mkBoxClass "Socket"
        [ "close"   !!! (undefined :: Socket -> IO ())
        --, "connect" ... undefined -- +1046
        --, "listen"  ... undefined
        ] 
    fromObj (MkInvocant x _) = undefined


mkBoxClass cls methods = newMI $ emptyMI
    { clsPublicMethods = newCollection' methodName $ map mkBoxMethod methods
    , clsName = cls
    }

mkBoxMethod (meth, fun) = AnyMethod $ MkSimpleMethod
    { smName = meth
    , smDefinition = MkMethodCompiled $ HsCode $ \args -> do
        str <- fromInvocant args
        fun str   -- Note that we expect "fun" to be monadic
    }

-- Hmmm, shall we make combinations for take care of things like a -> b -> a,
-- a -> m (), and other combinations?
instance (Boxable m a, Boxable m b, Boxable m c) => Boxable m (a, b, c) where
    classOf = const (newMI emptyMI)
    fromObj (MkInvocant x _) = undefined


-- (...) is used for non-monadic functions, and (!!!) for monadic ones
-- would be nice to make (...) work for both cases.
(...) x y = (x, mkObj . y)
(!!!) x y = (x, mkObjM . y)

mkObj :: (Show a, Boxable m a) => a -> m (Invocant m)
mkObj x = return $ MkInvocant x (class_interface (classOf x))

mkObjM :: (Show a, Boxable m a) => m a -> m (Invocant m)
mkObjM x = do
    x' <- x
    return $ MkInvocant x' (class_interface (classOf x'))

inv ./ meth = ivDispatch inv $ MkMethodInvocation meth (mkArgs [])

main = do
    jude <- mkObj "Hey Jude"

    rev_jude <- jude ./ "reverse"
    print rev_jude                       -- "eduJ yeH"

    print =<< (jude ./ "chop")           -- "Hey Jud"
    print =<< (jude ./ "uc")             -- "HEY JUDE"
    print =<< (jude ./ "lc")             -- "hey jude"
    print =<< (jude ./ "lcfirst")        -- "hey Jude"
    print =<< (rev_jude ./ "ucfirst")    -- "EduJ yeH"
    print =<< (rev_jude ./ "capitalize") -- "Eduj Yeh"
    
    things <- mkObj "lot$ of thing$"
    print =<< (things ./ "quotemeta")    -- "lot\\$\\ of\\ thing\\$"

    print =<< (jude ./ "split")          -- ["Hey","Jude"]

    things ./ "test"                     -- prints "lot$ of thing$"

    eight <- jude ./ "chars"
    print eight                     -- 8
    print =<< (eight ./ "chr")      -- "\b"



{-

-- TODO: get more sugar for constructing this types
xxx = do
    let base = newMI $ emptyMI
              { clsPublicMethods = newCollection' methodName $ map AnyMethod [
                MkSimpleMethod
                { smName = "foo"
                , smDefinition = MkMethodCompiled $ HsCode (const (return $ mkObj ("foo", "boo", "blah")) )
                }
                ]
              , clsName = "base"  
              }
        sub = newMI $ emptyMI
            { clsParents = [AnyClass base]
            , clsPublicMethods = newCollection' methodName $ map AnyMethod [
                MkSimpleMethod
                { smName = "bar"
                , smDefinition = MkMethodCompiled $ HsCode (const (return $ mkObj "bar") )
                }
                ]
            , clsName = "sub"
            }
        sub2 = newMI $ emptyMI
             { clsParents = [AnyClass base]
             , clsPublicMethods = newCollection' methodName $ map AnyMethod [
                 MkSimpleMethod
                 { smName = "foo"
                 , smDefinition = MkMethodCompiled $ HsCode (const (return $ mkObj "haha, surprise"))
                 }
                 ]
             , clsName = "sub2"
             }
        base_box    = MkInvocant "base" $ class_interface base
        sub_box     = MkInvocant "sub"  $ class_interface sub
        sub2_box    = MkInvocant "sub2" $ class_interface sub2
        call_create = MkMethodInvocation
            { miName = "bless"
            , miArguments = mkArgs [mkObj "moose"]
            }
        call_foo = MkMethodInvocation
            { miName = "foo"
            , miArguments = mkArgs [mkObj "moose"]
            }
        call_bar = MkMethodInvocation
            { miName = "bar"
            , miArguments = mkArgs [mkObj "moose"]
            }

    -- Create instance of base
    base_obj_box <- ivDispatch base_box call_create
    print base_obj_box

    -- Create instance of sub
    sub_obj_box <- ivDispatch sub_box call_create
    print sub_obj_box

    -- Create instance of sub2
    sub2_obj_box <- ivDispatch sub2_box call_create
    print sub2_obj_box

    -- Call foo on base class => would work, because foo is instance method
    print =<< ivDispatch base_box call_foo

    -- Call foo on base class => would work, because foo is instance method
    print =<< ivDispatch base_box call_bar

-}
