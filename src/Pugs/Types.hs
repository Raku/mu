{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

{-|
    Implementation Types.

>   Three Rings for the Elven-kings under the sky,
>   Seven for the Dwarf-lords in their halls of stone,
>   Nine for Mortal Men doomed to die,
>   One for the Dark Lord on his dark throne
>   In the Land of Mordor where the Shadows lie.
-}

module Pugs.Types (
    Type(..), mkType, anyType, showType, isaType, isaType', deltaType,
    ClassTree, initTree, addNode,

    Cxt(..), 
    cxtItem, cxtSlurpy, cxtVoid, cxtItemAny, cxtSlurpyAny,
    typeOfCxt, isSlurpyCxt, isItemCxt, isVoidCxt,
    enumCxt, cxtEnum,

    Var, 

    VStr, VBool, VInt, VRat, VNum, VComplex, VHandle, VSocket,
    VThread(..),

    MatchPGE(..)
) where
import Pugs.Internals

data Type
    = MkType !String      -- ^ A regular type
    | TypeOr  !Type !Type -- ^ The disjunction (|) of two types
    | TypeAnd !Type !Type -- ^ The conjunction (&) of two types
    deriving (Eq, Ord, Typeable)

instance Show Type where
    show t = "(mkType \"" ++ showType t ++ "\")"

showType :: Type -> String
showType (MkType typ)    = typ
showType (TypeOr t1 t2)  = showType t1 ++ "|" ++ showType t2
showType (TypeAnd t1 t2) = showType t1 ++ "&" ++ showType t2

type ClassTree = Tree Type


data Cxt = CxtVoid         -- ^ Context that isn't expecting any values
         | CxtItem !Type   -- ^ Context expecting a value of the specified type
         | CxtSlurpy !Type -- ^ Context expecting multiple values of the
                           --     specified type
    deriving (Eq, Show, Ord)

anyType :: Type
anyType = mkType "Any"

cxtItem   :: String -> Cxt
cxtItem   = CxtItem . mkType
cxtSlurpy :: String -> Cxt
cxtSlurpy = CxtSlurpy . mkType
cxtVoid   :: Cxt
cxtVoid   = CxtVoid

typeOfCxt :: Cxt -> Type
typeOfCxt CxtVoid           = anyType
typeOfCxt (CxtItem typ)     = typ
typeOfCxt (CxtSlurpy typ)   = typ

-- | Return a 'Cxt' indicating a context expecting a scalar of any type
cxtItemAny :: Cxt
cxtItemAny   = CxtItem anyType
-- | Return a 'Cxt' indicating a context expecting a list of any type
cxtSlurpyAny :: Cxt
cxtSlurpyAny = CxtSlurpy anyType

{-|
Return true if the given 'Cxt' (context) is 'CxtSlurpy', rather than
'CxtItem' or 'CxtVoid'.
-}
isSlurpyCxt :: Cxt -> Bool
isSlurpyCxt (CxtSlurpy _) = True
isSlurpyCxt _             = False
{-|
Return true if the given 'Cxt' (context) is 'CxtItem', rather than
'CxtSlurpy' or 'CxtVoid'.
-}
isItemCxt :: Cxt -> Bool
isItemCxt   (CxtItem _)   = True
isItemCxt   _             = False
{-|
Return true if the given 'Cxt' (context) is 'CxtVoid', rather than
'CxtSlurpy' or 'CxtItem'.
-}
isVoidCxt :: Cxt -> Bool
isVoidCxt   CxtVoid       = True
isVoidCxt   _             = False

-- | Return the Perl 5 calling convention bit value for the context.
enumCxt :: (Num a) => Cxt -> a
enumCxt CxtVoid       = 128
enumCxt (CxtItem _)   = 0
enumCxt (CxtSlurpy _) = 1

-- | Return the 'Cxt' corresponding to the given P5 calling convention bits.
cxtEnum :: (Show a, Num a) => a -> Cxt
cxtEnum 128 = CxtVoid
cxtEnum 0   = cxtItemAny
cxtEnum 1   = cxtSlurpyAny
cxtEnum n   = error ("Invalid cxt: " ++ show n)

{-|
Make a type value representing the type with the specified name.

Recognises conjunctive (&) and disjunctive (|) types.
-}
mkType :: String -- ^ Name of the type, e.g. \"Hash\" or \"Str|Int\"
       -> Type
mkType str
    | (t1, (_:t2)) <- span (/= '|') str
    = TypeOr (MkType t1) (mkType t2)
    | (t1, (_:t2)) <- span (/= '&') str
    = TypeAnd (MkType t1) (mkType t2)
    | otherwise
    = MkType str

-- | Variable name.
type Var   = String
-- | Uses Haskell's underlying representation for strings.
type VStr  = String
-- | Uses Haskell's underlying representation for booleans.
type VBool = Bool
-- | Uses Haskell's underlying representation for integers.
type VInt  = Integer
-- | Uses Haskell's underlying representation for rational numbers.
type VRat  = Rational
-- | Uses Haskell's 'Double' type to represent arbitrary numbers.
type VNum  = Double
-- | Uses Haskell's underlying representation for complex numbers.
type VComplex = Complex VNum
-- | Uses Haskell's underlying representation for filehandles.
type VHandle = Handle
-- | Uses Haskell's underlying representation for sockets.
type VSocket = Socket
-- | Uses Haskell's underlying representation for threads.
data VThread a = MkThread
    { threadId      :: ThreadId
    , threadLock    :: TMVar a
    }
    deriving (Show, Eq, Ord, Typeable)

-- | Rule Match object from PGE
data MatchPGE
    = PGE_Match !Int !Int ![MatchPGE] ![(VStr, MatchPGE)]
    | PGE_Array ![MatchPGE]
    | PGE_String !String
    | PGE_Fail
    deriving (Show, Eq, Ord, Read, Typeable)

instance Ord VHandle where
    compare _ _ = EQ
instance Ord VSocket where
    compare x y = compare (show x) (show y)
instance (Ord a) => Ord (Tree a) where
    compare _ _ = EQ
instance Ord (TMVar a) where
    compare x y = compare (show x) (show y)
instance Eq (TMVar a) where
    _ == _ = True
instance Show (TMVar a) where
    show _ = "<tmvar>"

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
addNode (Node obj [Node any (Node item ns:rest), junc]) typ =
    Node obj [Node any (Node item ((Node typ []):ns):rest), junc]
addNode _ _ = error "malformed tree"

{-|
Default class tree, containing all built-in types.
-}
initTree :: ClassTree
initTree = fmap MkType $ Node "Object"
    [ Node "Any"
        [ Node "Item"
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
                    , Node "Block" []
                    ]
                , Node "Rul" []
                , Node "Pugs::Internals::VRule" []
                , Node "Match" []
                , Node "Scalar::Const" []
                , Node "Scalar::Proxy" []
                , Node "Scalar::Lazy" []
                , Node "Scalar::Perl5" []
                , Node "Proxy" []
                , Node "Control::Caller" []
                , Node "Time::Local" []
                , Node "Type"
                    [ Node "Package"
                        [ Node "Module"
                            [ Node "Class"
                                [ Node "Role" []
                                , Node "Grammar" []
                                ] ] ] ]
                ]
            ]
        , Node "Pair" []
        ]
    , Node "Junction" [] ]

