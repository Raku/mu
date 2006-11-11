{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

import MO.Run
import MO.Compile
import MO.Compile.Class
import MO.Util
import Data.Typeable
import Pugs.Val
import Pugs.Pretty
import Pugs.Internals

say = putStrLn

{-
mkBox :: String -> Invocant IO
mkBox = undefined
    { clsPublicMethods = newCollection' name $ map AnyMethod [
        MkSimpleMethod
        { smName = "foo"
        , smDefinition = MkMethodCompiled $ PureCode (const (mkObj ("foo", "boo", "blah")) )
        }
        ]
        , clsName = "base"  
    }
-}

-- A cast-map

class (Typeable a, Ord a, Typeable1 m, Monad m) => Boxable m a | a -> m where
    classOf :: a -> MI m
    fromObj :: Invocant m -> m a

-- XXX - Once MI for native types is made generally "is open" this must be adjusted as well.
instance Boxable IO String where
    classOf _ = mkBoxClass "Str"
        [ "reverse" ... (reverse :: String -> String)
        , "chars"   ... length
        ]
    fromObj (MkInvocant x _) = undefined

instance Boxable IO Int where
    classOf _ = mkBoxClass "Int"
        [ "chr"     ... ((:[]) . chr)
        ]

instance Boxable IO Char where

(...) x y = (x, mkObj . y)

mkBoxClass cls methods = newMI $ emptyMI
    { clsPublicMethods = newCollection' methodName $ map mkBoxMethod methods
    , clsName = cls
    }

mkBoxMethod (meth, fun) = AnyMethod $ MkSimpleMethod
    { smName = meth
    , smDefinition = MkMethodCompiled $ HsCode $ \args -> do
        str <- fromInvocant args
        return (fun str)
    }

instance (Boxable m a, Boxable m b, Boxable m c) => Boxable m (a, b, c) where
    classOf = const (newMI emptyMI)

mkObj :: (Show a, Boxable m a) => a -> Invocant m
mkObj x = MkInvocant x (class_interface (classOf x))

inv ./ meth = ivDispatch inv $ MkMethodInvocation meth (mkArgs [])

main = do
    let jude = mkObj "Hey Jude"
    print =<< (jude ./ "reverse")   -- "eduJ yeH"

    eight <- jude ./ "chars"
    print eight                     -- 8
    print =<< (eight ./ "chr")      -- "\b"

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
