{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

import MO.Run
import MO.Compile
import MO.Compile.Class
import MO.Util
import MO.Compile.Role
import Control.Exception (try)
import MO.Compile.Attribute

import Data.List (sort)
import Data.Typeable

say = putStrLn


no_conflict = newMI $ emptyMI 
            { clsName = "no_conflict"
            , clsParents = []
            , clsRoles =
                [ make_role [] [make_method ("foo", "foo")] []
                , make_role [] [make_method ("foo2", "foo2")] [] 
                ]
            }


shadowed    = newMI $ emptyMI 
            { clsName = "shadowed"
            , clsParents = []
            , clsRoles = [
                make_role [] [make_method ("foo", "foo")]
                    [ make_role [] [make_method ("foo", "foo2")] []
                    , make_role [] [make_method ("bar", "bar")]  []
                    ]
                ]
            }


shadowed_a  = newMI $ emptyMI 
            { clsName = "shadowed_a"
            , clsParents = []
            , clsRoles = [
                make_role [mkAttributeStub "foo"] []
                    [ make_role [mkAttributeStub "foo"] [] []
                    ]
                ]
            }


no_conflict_a = newMI $ emptyMI 
            { clsName = "no_conflict_a"
            , clsParents = []
            , clsRoles =
                [ make_role [mkAttributeStub "foo"] [] []
                , make_role [mkAttributeStub "bar"] [] [] 
                ]
            }

bar_as_foo = MkAttribute
    { attrName          = "foo"
    , attrAccessorName  = "bar"
    , attrIsPrivate     = False
    , attrDefault       = return stubInvocant
    }

with_conflict_a = newMI $ emptyMI 
            { clsName = "with_conflict_a"
            , clsParents = []
            , clsRoles =
                [ make_role [mkAttributeStub "foo"] [] []
                , make_role [bar_as_foo] [] [] 
                ]
            }

with_conflict_bad = newMI $ emptyMI 
            { clsName = "with_conflict_bad"
            , clsParents = []
            , clsRoles =
                [ make_role [mkAttributeStub "bar"] [] []
                , make_role [bar_as_foo] [] [] 
                ]
            }


mkbox s c = MkInvocant s (class_interface c)

no_conflict_box = mkbox "no_conflict" no_conflict
with_conflict_box = mkbox "with_conflict" with_conflict
shadowed_box    = mkbox "shadowed" shadowed
shadowed_a_box    = mkbox "shadowed_a" shadowed_a
no_conflict_a_box = mkbox "no_conflict_a" no_conflict_a
with_conflict_a_box = mkbox "with_conflict_a" with_conflict_a
with_conflict_bad_box = mkbox "with_conflict_bad" with_conflict_bad

make_call s = MkMethodInvocation
            { miName = s
            , miArguments = mkArgs [__"moose"]
            }

make_instance box = do
    r' <- ivDispatch box (make_call "bless")
    say ("instance created: " ++ (show r'))
    return r'

check_methods c l = show (sort (all_methods c)) == show (sort l) 
check_attributes c l = show (sort (map attrName $ all_attributes c)) == show (sort l) 

ok c msg = if c then say ("ok - " ++ msg) else say ("NOT OK - " ++ msg)

call box c result msg = do
    rv <- try $ ivDispatch box c
    case rv of 
        Left e      -> say ("NOT OK - can't call " ++ msg ++ " - Error: " ++ show e )
        Right r'    -> ok ((show r') == (show result)) msg

make_methods :: (Typeable1 m, Monad m) => [(String, String)] -> [AnyMethod m]
make_methods = map make_method

make_method :: (Typeable1 m, Monad m) => (String, String) -> AnyMethod m
make_method (n,n') = AnyMethod (
                      MkSimpleMethod { smName = n 
                                     , smDefinition = MkMethodCompiled $ PureCode (const (__(n')))
                                     })

make_role as ms rs = emptyRole
                 { roPublicMethods  = newCollection' name ms
                 , roAttributes     = as
                 , roRoles          = rs
                 }


with_conflict = newMI $ emptyMI 
            { clsName = "with_conflict"
            , clsParents = []
            , clsRoles =
                [ make_role [] [make_method ("foo", "foo")] []
                , make_role [] [make_method ("foo", "foo2")] [] 
                ]
            }
ok_conflict f = do
    rv  <- try f
    case rv of
        Left e  -> putStrLn ("ok - It worked, yay! The error messages was: " ++ show e)
        _       -> putStrLn "NOT OK - Darn, it didn't work."



main = do
    -- Create instances
    no_conflict_i       <- make_instance no_conflict_box
    with_conflict_i     <- make_instance with_conflict_box
    shadowed_i          <- make_instance shadowed_box
    shadowed_a_i        <- make_instance shadowed_a_box
    no_conflict_a_i     <- make_instance no_conflict_a_box
    with_conflict_a_i   <- make_instance with_conflict_a_box
    with_conflict_bad_i <- make_instance with_conflict_bad_box
  
    ok (check_methods no_conflict ["bless"])
       "class methods of no_conflict"
    ok (check_methods no_conflict ["foo","foo2"])
       "instance methods of no_conflict"

    ok (check_methods shadowed ["bless"])
       "class methods of shadowed"
    ok (check_methods shadowed ["foo","bar"])
       "instance methods of shadowed"

    call shadowed_i (make_call "foo") "foo" "calling shadowed method foo"
    call shadowed_i (make_call "bar") "bar" "calling non-shadowed method bar" 

    ok (check_methods with_conflict ["bless"])
       "class methods of with_conflict"

    ok (check_methods shadowed_a ["foo"])
       "instance methods of shadowed_a"
    ok (check_attributes shadowed_a ["foo","foo"])
       "attributes of shadowed_a"
    
    ok (check_methods no_conflict_a ["foo","bar"])
       "instance methods of no_conflict_a"
    ok (check_attributes no_conflict_a ["foo","bar"])
       "attributes of no_conflict_a"

    ok (check_methods with_conflict_a ["foo","bar"])
       "instance methods of with_conflict_a"
    ok (check_attributes with_conflict_a ["foo","foo"])
       "attributes of with_conflict_a"
  
    ok (check_attributes with_conflict_bad ["foo","bar"])
       "attributes of with_conflict_bad"
    
    say "# lets try make a MERGE CONFLICT:"
    ok_conflict (print $ all_methods with_conflict_bad)
    ok_conflict (print $ all_methods with_conflict)
