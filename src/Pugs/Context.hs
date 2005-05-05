{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Context propagators and equalizers.

>   Tree and flower and leaf and grass,
>   Let them pass! Let them pass!
>   Hill and water under sky,
>   Pass them by! Pass them by!
-}

module Pugs.Context where
import Pugs.Internals
import Pugs.Types

countTree :: Tree Type -> Int
countTree (Node _ []) = 0
countTree (Node _ cs) = 1 + sum (map countTree cs)

deltaType :: ClassTree -> Type -> Type -> Int
deltaType = junctivate min max $ \tree base target ->
    let distance = distanceType tree base target in
    if distance < 0
        then countTree tree - distance
        else distance

junctivate :: (t -> t -> t) -> (t -> t -> t)
              -> (ClassTree -> Type -> Type -> t)
              -> ClassTree -> Type -> Type -> t
junctivate or and f tree base target
    | TypeOr t1 t2 <- target
    = redo base t1 `or` redo base t2
    | TypeOr b1 b2 <- base
    = redo b1 target `or` redo b2 target
    | TypeAnd t1 t2 <- target
    = redo base t1 `and` redo base t2
    | TypeAnd b1 b2 <- base
    = redo b1 target `and` redo b2 target
    | otherwise
    = f tree base target
    where
    redo = junctivate or and f tree

-- When saying Int.isa(Scalar), Scalar is the base, Int is the target
isaType :: ClassTree -> String -> Type -> Bool
isaType tree base target = isaType' tree (mkType base) target

isaType' :: ClassTree -> Type -> Type -> Bool
isaType' = junctivate (||) (&&) $ \tree base target ->
    distanceType tree base target > 0

-- XXX -- Junctive Types -- XXX --
distanceType :: ClassTree -> Type -> Type -> Int
distanceType tree base target
--  | not (castOk base target)  = 0
    | otherwise         = distance
    where
    distance  = compareList l1 l2
    l1 = findList base tree
    l2 = findList target tree

castOk :: a -> b -> Bool
castOk _ _ = True

compareList :: [Type] -> [Type] -> Int
compareList [] _ = 0
compareList _ [] = 0
compareList l1 l2
    | last l1 `elem` l2 =   length(l2 \\ l1) + 1
    | last l2 `elem` l1 = - length(l1 \\ l2) - 1
    | otherwise = compareList l1 (init l2)

findList :: Type -> Tree Type -> [Type]
findList base (Node l cs)
    | base == l                             = [l]
    | Just ls <- find (not . null) found    = l:ls
    | otherwise                             = []
    where
    found = map (findList base) cs

prettyTypes :: String
prettyTypes = drawTree $ fmap show initTree

initTree :: Tree Type
initTree = fmap MkType $ Node "Any" [ Node "Void"
    [ Node "Object"
        [ Node "List"
            [ Node "Lazy"
                [ Node "Array"
                    [ Node "Array::Const" []
                    , Node "Array::Slice" []
                    ]
                , Node "Hash"
                    [ Node "Hash::Const" []
                    , Node "Hash::Env" []
                    ]
                ]
            , Node "Eager" []
            ]
        , Node "Pair" []
        , Node "Scalar"
            [ Node "Complex"
                [ Node "Num"
                    [ Node "Rat"
                        [ Node "Int"
                            [ Node "Bit" [] ] ] ] ]
            , Node "Bool" []
            , Node "Str" []
            , Node "Ref" []
            , Node "IO" []
            , Node "Socket" []
            , Node "Thread" []
            , Node "Code"
                [ Node "Routine"
                    [ Node "Sub"
                        [ Node "Method" []
                        , Node "Submethod" []  -- why isn't this a node off Method? - mugwump
                        ]
                    , Node "Macro" [] ]
                , Node "Block"
                    [ Node "Bare"
                        [ Node "Parametric" [] ] ]
                ]
            , Node "Rule" []
            , Node "Junction" []
            , Node "Scalar::Const" []
            , Node "Scalar::Proxy" []
            , Node "Scalar::Lazy" []
            ]
        ]
    , Node "Grammar" []
    , Node "Package"
        [ Node "Module"
            [ Node "Class" [] ] ]
    , Node "Action" []
    , Node "Trait"
        [ Node "PkgTrait" [] ] ] ]

