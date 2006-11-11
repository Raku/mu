{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

import MO.Run
import MO.Compile
import MO.Compile.Class
import MO.Compile.Attribute
import MO.Util
import Debug.Trace

import Data.List (sort)
import Data.Typeable

say = putStrLn

base = newMI $ emptyMI { clsName = "base" }

point = newMI $ emptyMI 
      { clsName = "point"
      , clsParents = [AnyClass base]
      , clsAttributes = [mkAttribute "x" (__"defaultX"), mkAttribute "y" (__"defaultY")]
      , clsPublicMethods = newCollection' methodName $ map AnyMethod [
        MkSimpleMethod
        { smName = "distance"
        , smDefinition = MkMethodCompiled $ PureCode (const (__("distance defined in point")))
        }]
      }

point3d = newMI $ emptyMI 
        { clsName = "point3d"
        , clsParents = [AnyClass point]
        , clsAttributes = [mkAttribute "z" (__"defaultZ")]
--      , clsPublicMethods = newCollection' methodName $ map AnyMethod [
--        MkSimpleMethod
--        { smName = "z"
--        , smDefinition = MkMethodCompiled
--            { mcBody = PureCode (const (__("z defined in point3d"))) }
--        }]
        }


colorful = newMI $ emptyMI
         { clsName = "colorful"
         , clsParents = [AnyClass base]
         , clsAttributes = [mkAttribute "color" (__"prettyColor")]
--       , clsPublicMethods = newCollection' methodName $ map AnyMethod [
--         MkSimpleMethod
--         { smName = "color"
--         , smDefinition = MkMethodCompiled
--             { mcBody = PureCode (const (__("color defined in colorful"))) }
--         }]
         }

colorful_point = newMI $ emptyMI
               { clsName = "colorful_point"
               , clsParents = [AnyClass point, AnyClass colorful]
               }

colorful_point3d = newMI $ emptyMI
                 { clsName = "colorful_point3d"
                 , clsParents = [AnyClass point3d, AnyClass colorful]
                 }

mkbox :: forall m. (Typeable1 m, Monad m) => String -> MI m -> Invocant m
mkbox s c = MkInvocant s (class_interface c)

base_box = mkbox "base" base
point_box = mkbox "point" point
point3d_box = mkbox "point3d" point3d
colorful_box = mkbox "colorful" colorful
colorful_point_box = mkbox "colorful_point" colorful_point
colorful_point3d_box = mkbox "colorful_point3d" colorful_point3d


call :: (Typeable a, Ord a, Show a, Typeable1 m, Monad m) => String -> [a] -> MethodInvocation m
call s args = MkMethodInvocation
            { miName = s
            , miArguments = mkArgs (map __ args)
            }

call0 :: (Typeable1 m, Monad m) => String -> MethodInvocation m
call0 s = MkMethodInvocation
            { miName = s
            , miArguments = mkArgs []
            }

make_instance0 box = do
    r' <- ivDispatch box (call0 "bless")
    say ("instance created: " ++ (show r'))
    return r'

check_methods c l = show (sort (all_methods c)) == show (sort l)

ok c msg = if c then say ("ok - " ++ msg) else say ("NOT OK - " ++ msg)

main = do
    -- Create instances
    base_i              <- make_instance0 base_box
    point_i             <- make_instance0 point_box
    point3d_i           <- make_instance0 point3d_box
    colorful_i          <- make_instance0 colorful_box
    colorful_point_i    <- make_instance0 colorful_point_box
    colorful_point3d_i  <- make_instance0 colorful_point3d_box
   
    let precedences = map class_precedence_list
            [base, point, point3d, colorful, colorful_point, colorful_point3d]
        correct = map (map AnyClass) [[base], [point,base], [point3d,point,base]
                   , [colorful,base], [colorful_point,point,colorful,base]
                   , [colorful_point3d,point3d,point,colorful,base]]
    if precedences == correct
        then say "ok - Precedences are fine."
        else say "NOT OK - precedences are wrong."
    let matrix =
            [ (base,             ["bless"]                                      )
            , (point,            ["bless", "distance", "x", "y"]                )
            , (point3d,          ["bless", "distance", "x", "y", "z"]           )
            , (colorful,         ["bless", "color"]                             )
            , (colorful_point,   ["bless", "distance", "x", "y", "color"]       )
            , (colorful_point3d, ["bless", "distance", "x", "y", "color", "z"]  )
            ]

    mapM_ (\(c,ms) -> do
        ok (check_methods c ms) (class_name c ++ " instance methods")
        ) matrix

