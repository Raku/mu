{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

{-|
    Context propagators and equalizers.

>   Tree and flower and leaf and grass,
>   Let them pass! Let them pass!
>   Hill and water under sky,
>   Pass them by! Pass them by!

(This module could probably use a name change...)
-}

module Pugs.Context (
    deltaType,
    isaType,
    addNode,
    initTree
) where
import Pugs.Internals
import Pugs.Types

{-|
Count the total number of types in a class tree, including both internal and
leaf nodes.

This is used by 'deltaType' to ensure that incompatible types are always
further apart than compatible types.
-}
countTree :: ClassTree -> Int
countTree (Node _ []) = 1
countTree (Node _ cs) = 1 + sum (map countTree cs)

{-|
Find the \'difference\' between two types in the given class tree (for MMD
purposes and such).

Identical types (that exist in the class tree) produce 0. Compatible types
will produce a small positive number representing their distance.
Incompatible will produce a distance larger
than any two compatible types. If one (or both) of the types doesn't exist in
the tree, the result is a very large number.

> <scook0> is deltaType supposed to be returning large positive numbers for 
>            types that are actually incompatible?
> <autrijus> that is a open design question.
> <autrijus> it is that way because we want
> <autrijus> '1'+'2'
> <autrijus> to work
> <scook0> I see
> <autrijus> without having to define <+> as Scalar Scalar
> <autrijus> I think I did think of leaving a compatibleTypes as remedy
> <autrijus> to specify things that are fundamentally uncastable
> <scook0> I think I'll just document the current behaviour for now
> <autrijus> nod. it is a mess. it really wants a rewrite.
-}
deltaType :: ClassTree -- ^ Class tree to use for the comparison
          -> Type      -- ^ Base type
          -> Type      -- ^ Possibly-derived type
          -> Int
deltaType = junctivate min max $ \tree base target ->
    let distance = distanceType tree base target in
    if distance < 0
        then countTree tree - distance
        else distance

{-|
Autothreading of comparisons between junctive types.

Just as autothreading over value junctions will perform an operation on all
junction elements and combine the results back into a junction, this function
autothreads some type comparison over all the possible type permutations,
then combines the results using two user-specified /functions/.

E.g. if we want to check whether the type @(Int|Str)@ is a @Num@, we first
check whether @Int@ is a @Num@ (@True@), then check whether @Str@ is a num
(@False@), then combine the results using the specified disjunctive combiner
(in this case Haskell's @(||)@). The result is thus @True@.
-}
junctivate :: (t -> t -> t) -- ^ Function to combine results over disjunctive 
                            --     (@|@) types
           -> (t -> t -> t) -- ^ Function to combine results over conjunctive 
                            --     (@\&@) types
           -> (ClassTree -> Type -> Type -> t)
                            -- ^ Function that will actually perform the 
                            --     comparison (on non-junctive types)
           -> ClassTree     -- ^ Class tree to pass to the comparison function
           -> Type          -- ^ First type to compare
           -> Type          -- ^ Second type to compare
           -> t
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
{-|
A more convenient version of 'isaType\'' that automatically converts the base
type string into an actual 'Type' value.
-}
isaType :: ClassTree -- ^ Class tree to use for the comparison
        -> String    -- ^ Base type
        -> Type      -- ^ Possibly-derived type
        -> Bool
isaType tree base target = isaType' tree (mkType base) target

{-|
Return true if the second type (the \'target\') is derived-from or equal-to the 
first type (the \'base\'), in the context of the given class tree.

This function will autothread over junctive types.
-}
isaType' :: ClassTree -- ^ Class tree to use for the comparison
         -> Type      -- ^ Base type
         -> Type      -- ^ Possibly-derived type
         -> Bool
isaType' = junctivate (||) (&&) $ \tree base target ->
    distanceType tree base target >= 0

{-|
Compute the \'distance\' between two types by applying 'findList' to each of
them, and passing the resulting type chains to 'compareList'.

See 'compareList' for further details.
-}
distanceType :: ClassTree -> Type -> Type -> Int
distanceType tree base target = compareList l1 l2
--  | not (castOk base target)  = 0
--  | otherwise = compareList l1 l2
    where
    l1 = findList base tree
    l2 = findList target tree

{-
-- | (This is currently unused...)
castOk :: a -> b -> Bool
castOk _ _ = True
-}

{-|
Take two inheritance chains produced by 'findList', and determine how
\'compatible\' the first one is with the second.

Compatible types will produce a number indicating how distant they are.
Incompatible types produce a negative number indicating how much the base type
would need to be relaxed. If one (or both) types doesn't exist in the tree, a 
large negative number is produced

E.g.:

* comparing @Int@ and @Int@ will produce 0

* comparing @Scalar@ and @String@ will produce 1

* comparing @Num@ and @Scalar@ will produce -2

* comparing @Blorple@ and @Method@ will produce -999 (or similar)
-}
compareList :: [Type] -- ^ Base type's chain
            -> [Type] -- ^ Possibly-derived type's chain
            -> Int
compareList [] _ = -999 -- XXX hack (nonexistent base type?)
compareList _ [] = -999 -- XXX hack (incompatible types)
compareList l1 l2
    | last l1 `elem` l2 =   length(l2 \\ l1) -- compatible types
    | last l2 `elem` l1 = - length(l1 \\ l2) -- anti-compatible types
    | otherwise = compareList l1 (init l2)

{-|
Produce the type \'inheritance\' chain leading from the base type (@Any@) to
the given type.

e.g.

@
'findList' ('MkType' \"Num\") 'initTree'
@

will produce the list of types

@
Any, Void, Object, Scalar, Complex, Num
@

This function does /not/ expect to be given junctive types.
-}
findList :: Type      -- ^ 'Type' to find the inheritance chain of
         -> ClassTree -- ^ Class tree to look in
         -> [Type]
findList base (Node l cs)
    | base == l                             = [l]
    | Just ls <- find (not . null) found    = l:ls
    | otherwise                             = []
    where
    found :: [[Type]]
    found = map (findList base) cs

{-
{-|
Pretty-print the initial class tree, using @Tree@'s @drawTree@.

(This seems to be a debugging aid, since it's not actually used anywhere.)
-}
prettyTypes :: String
prettyTypes = drawTree $ fmap show initTree
-}

{-|
Add a new \'top-level\' type to the class tree, under @Object@.
-}
addNode :: ClassTree -> Type -> ClassTree
addNode (Node any [Node void (Node obj ns:rest)]) typ =
    Node any [Node void (Node obj ((Node typ []):ns):rest)]
addNode _ _ = error "malformed tree"

{-|
Default class tree, containing all built-in types.
-}
initTree :: ClassTree
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
            , Node "IO"
                [ Node "IO::Dir" []
                ]
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
            , Node "Match" []
            , Node "Junction" []
            , Node "Scalar::Const" []
            , Node "Scalar::Proxy" []
            , Node "Scalar::Lazy" []
            , Node "Scalar::Perl5" []
            , Node "Proxy" []
	    ]
        ]
    , Node "Grammar" []
    , Node "Type"
        [ Node "Package"
            [ Node "Module"
                [ Node "Class" [] ] ]
        , Node "Trait"
            [ Node "PkgTrait" [] ] ] ] ]

