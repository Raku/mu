{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Context propagators and equalizers.

    Tree and flower and leaf and grass,
    Let them pass! Let them pass!
    Hill and water under sky,
    Pass them by! Pass them by!
-}

module Context where
import Internals

type Cxt = String

countTree :: Tree Type -> Int
countTree (Node _ []) = 0
countTree (Node _ cs) = 1 + sum (map countTree cs)

deltaType :: ClassTree -> Type -> Type -> Int
deltaType tree base target
    | distance < 0 = countTree tree - distance
    | otherwise = distance
    where
    distance = distanceType tree base target 

-- When saying Int.isa(Scalar), Scalar is the base, Int is the target
isaType :: ClassTree -> Type -> Type -> Bool
isaType tree base target = distanceType tree base target > 0

-- XXX -- Junctive Types -- XXX --
distanceType :: ClassTree -> Type -> Type -> Int
distanceType tree base target
    | not (castOk base target)  = 0
    | otherwise         = distance
    where
    distance  = compareList l1 l2
    l1 = findList base tree
    l2 = findList target tree

castOk :: a -> b -> Bool
castOk _ _ = True

compareList :: (Eq a) => [a] -> [a] -> Int
compareList [] _ = 0
compareList _ [] = 0
compareList l1 l2
    | last l1 `elem` l2 =   length(l2 \\ l1) + 1
    | last l2 `elem` l1 = - length(l1 \\ l2) - 1
    | otherwise = compareList l1 (init l2)

findList :: (Eq a) => [a] -> Tree [a] -> [[a]]
findList [] _ = []
findList base (Node l cs)
    | base == l                                = [l]
    | Just ls <- find (not . null) found    = l:ls
    | otherwise                             = []
    where
    found = map (findList base) cs

prettyTypes :: String
prettyTypes = drawTree initTree

type ClassTree = Tree Type
type Type = String

initTree :: Tree Type
initTree = Node "Any"
    [ Node "Object"
        [ Node "List"
            [ Node "Lazy"
                [ Node "Array" []
                , Node "Hash" [] ]
            , Node "Eager" []
            ]
        , Node "Scalar"
            [ Node "Complex"
                [ Node "Num"
                    [ Node "Rat"
                        [ Node "Int"
                            [ Node "Bit" [] ] ] ] ]
            , Node "Bool" []
            , Node "Str" []
            , Node "Ref" []
            , Node "Pair" []
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
            ]
        ]
    , Node "Grammar" []
    , Node "Package"
        [ Node "Module"
            [ Node "Class" [] ] ]
    , Node "Action" []
    , Node "Void" []
    , Node "Trait"
        [ Node "PkgTrait" [] ] ]

