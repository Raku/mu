import MO.Run
import MO.Compile
import MO.Compile.Class
import MO.Util

say = putStrLn


-- TODO: get more sugar for constructing this types
main = do
    let base = newMI $ emptyMI
              { clsPublicMethods = newCollection' name $ map AnyMethod [
                MkSimpleMethod
                { smName = "foo"
                , smDefinition = MkMethodCompiled $ PureCode (const (__("foo", "boo", "blah")) )
                }
                ]
              , clsName = "base"  
              }
        sub = newMI $ emptyMI
            { clsParents = [AnyClass base]
            , clsPublicMethods = newCollection' name $ map AnyMethod [
                MkSimpleMethod
                { smName = "bar"
                , smDefinition = MkMethodCompiled $ PureCode (const (__"bar") )
                }
                ]
            , clsName = "sub"
            }
        sub2 = newMI $ emptyMI
             { clsParents = [AnyClass base]
             , clsPublicMethods = newCollection' name $ map AnyMethod [
                 MkSimpleMethod
                 { smName = "foo"
                 , smDefinition = MkMethodCompiled $ PureCode (const (__"haha, surprise"))
                 }
                 ]
             , clsName = "sub2"
             }
        base_box    = MkInvocant "base" $ class_interface base
        sub_box     = MkInvocant "sub"  $ class_interface sub
        sub2_box    = MkInvocant "sub2" $ class_interface sub2
        call_create = MkMethodInvocation
            { miName = "bless"
            , miArguments = mkArgs [__"moose"]
            }
        call_foo = MkMethodInvocation
            { miName = "foo"
            , miArguments = mkArgs [__"moose"]
            }
        call_bar = MkMethodInvocation
            { miName = "bar"
            , miArguments = mkArgs [__"moose"]
            }

    -- Create instance of base
    let Just base_obj_box = ivDispatch base_box call_create
    print base_obj_box

    -- Create instance of sub
    let Just sub_obj_box = ivDispatch sub_box call_create
    print sub_obj_box

    -- Create instance of sub2
    let Just sub2_obj_box = ivDispatch sub2_box call_create
    print sub2_obj_box

    -- Call foo on base class => would work, because foo is instance method
    case ivDispatch base_box call_foo of
        Nothing -> say "NOT OK, called foo on base and returned nothing"
        Just _  -> say "ok, instance method being called on class call"

    -- Call foo on sub class => would work too
    case ivDispatch sub_box call_foo of
        Nothing -> say "NOT OK, foo on sub class"
        Just _  -> say "ok, fo on sub class"    

    -- foo on object of base
    case ivDispatch base_obj_box call_foo of
        Nothing -> say "NOT OK"
        Just  r -> do
            let r' = r
            say $ "ok calling foo " ++ (show r')

    -- bar on object of base
    case ivDispatch base_obj_box call_bar of
        Nothing -> say "ok"
        Just _  -> say "NOT OK, found bar in a base object"

    -- foo on object of sub
    case ivDispatch sub_obj_box call_foo of
        Nothing -> say "NOT OK"
        Just  r -> do
            let r' = r
            say $ "ok calling foo " ++ (show r')

    -- bar on object of sub
    case ivDispatch sub_obj_box call_bar of
        Nothing -> say "NOT OK, couldnt find bar on sub object"
        Just  r -> do
            let r' = r
            say $ "ok calling bar " ++ (show r')

    -- foo on object of sub2
    case ivDispatch sub2_obj_box call_foo of
        Nothing -> say "NOT OK, couldnt find foo on sub2 object"
        Just  r -> do
            let r' = r
            if (show r') == (show "haha, surprise")
                then say "ok"
                else say "NOT OK, got foo from parent wrongly"


    say "Class precedence lists"
    print $ class_precedence_list base
    print $ class_precedence_list sub
    print $ class_precedence_list sub2 

    --print (instance_interface sub :: MethodTable)
    --
    ---}
