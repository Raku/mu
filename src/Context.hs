{-# OPTIONS -fglasgow-exts #-}

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

countTree (Node _ []) = 0
countTree (Node _ cs) = 1 + sum (map countTree cs)

deltaType :: ClassTree -> Type -> Type -> Int
deltaType tree x y
    | d < 0     = countTree tree - d
    | otherwise = d
    where
    d = distanceType tree x y

isaType :: ClassTree -> Type -> Type -> Bool
isaType tree x y = distanceType tree x y > 0

-- XXX -- Junctive Types -- XXX --
distanceType :: ClassTree -> Type -> Type -> Int
distanceType tree x y
    | not (castOk x y)  = 0
    | otherwise         = d
    where
    d  = compareList l1 l2
    l1 = findList x tree
    l2 = findList y tree

castOk _ _ = True

compareList [] _ = 0
compareList _ [] = 0
compareList l1 l2
    | last l1 `elem` l2 =   length(l2 \\ l1) + 1
    | last l2 `elem` l1 = - length(l1 \\ l2) - 1
    | otherwise = compareList l1 (init l2)

findList [] _ = []
findList x (Node l cs)
    | x == l                                = [l]
    | Just ls <- find (not . null) found    = l:ls
    | otherwise                             = []
    where
    found = map (findList x) cs

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
            , Node "Code"
                [ Node "Routine"
                    [ Node "Sub"
                        [ Node "Method" []
                        , Node "Submethod" [] ]
                    , Node "Macro" [] ]
                , Node "Block"
                    [ Node "Bare"
                        [ Node "Parametric" [] ] ]
                ]
            , Node "Rule" []
            , Node "Junction" []
            , Node "LValue" [] -- XXX Wrong
            ]
        ]
    , Node "Grammar" []
    , Node "Package"
        [ Node "Module"
            [ Node "Class" [] ] ]
    , Node "Action" []
    , Node "Void" []
    ]
