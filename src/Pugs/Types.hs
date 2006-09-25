{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields -fallow-overlapping-instances #-} 
{-|
    Implementation Types.

>   Three Rings for the Elven-kings under the sky,
>   Seven for the Dwarf-lords in their halls of stone,
>   Nine for Mortal Men doomed to die,
>   One for the Dark Lord on his dark throne
>   In the Land of Mordor where the Shadows lie.
-}

module Pugs.Types 
{-
(
    Type(..), mkType, anyType, showType, isaType, isaType', deltaType,
    ClassTree, initTree, addNode,

    Cxt(..), 
    cxtItem, cxtSlurpy, cxtVoid, cxtItemAny, cxtSlurpyAny,
    typeOfCxt, isSlurpyCxt, isItemCxt, isVoidCxt,
    enumCxt, cxtEnum,

    VStr, VBool, VInt, VRat, VNum, VComplex, VHandle, VSocket,
    VThread(..),

    MatchPGE(..)
)
-}
where
import Pugs.Internals
import Data.Bits (shiftL)
import qualified Judy.StrMap as H
import qualified Judy.CollectionsM as C
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as Str

data Type
    = MkType !ID         -- ^ A regular type
    | TypeOr  !Type !Type -- ^ The disjunction (|) of two types
    | TypeAnd !Type !Type -- ^ The conjunction (&) of two types
    deriving (Eq, Ord, Typeable, Data)

instance ((:>:) ByteString) Type where
    cast (MkType x) = cast x
    cast (TypeOr t1 t2)  = cast t1 +++ __"|" +++ cast t2
    cast (TypeAnd t1 t2) = cast t1 +++ __"&" +++ cast t2

instance ((:>:) Type) ByteString where
    cast = MkType . cast

instance ((:>:) Type) Pkg where
    cast = cast . (cast :: Pkg -> ByteString)

instance ((:>:) Pkg) Type where
    cast = cast . (cast :: Type -> ByteString)

instance Show Type where
    show t = "(mkType \"" ++ showType t ++ "\")"

showType :: Type -> String
showType (MkType typ)    = cast typ
showType (TypeOr t1 t2)  = showType t1 ++ "|" ++ showType t2
showType (TypeAnd t1 t2) = showType t1 ++ "&" ++ showType t2

newtype ClassTree = MkClassTree (Tree Type)
    deriving (Eq, Ord, Typeable)

instance Show ClassTree where
    show t = "{ClassTree:" ++ show (countTree t) ++ "}"

data Cxt = CxtVoid         -- ^ Context that isn't expecting any values
         | CxtItem !Type   -- ^ Context expecting a value of the specified type
         | CxtSlurpy !Type -- ^ Context expecting multiple values of the
                           --     specified type
    deriving (Eq, Show, Ord, Typeable)

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
    = TypeOr (mkType t1) (mkType t2)
    | (t1, (_:t2)) <- span (/= '&') str
    = TypeAnd (mkType t1) (mkType t2)
    | otherwise
    = MkType (cast str)

data Var = MkVar
    { v_name    :: ID
    , v_sigil   :: VarSigil
    , v_twigil  :: VarTwigil
    , v_categ   :: VarCateg
    , v_package :: Pkg
    , v_meta    :: VarMeta
    }
    deriving (Eq, Ord, Typeable, Data)

-- | a dummy scalar, used for example as the invocant
-- in the signature :( $ : $x, $y ).
varNullScalar :: Var
varNullScalar = MkVar
    { v_name    = nullID
    , v_sigil   = SScalar
    , v_twigil  = TNil
    , v_categ   = CNil
    , v_package = MkPkg []
    , v_meta    = MNil
    }

data VarMeta
    = MNil
    | MFold             -- [+]
    | MScan             -- [\+]
    | MPre              -- >>+
    | MPost             -- +<<
    | MHyper            -- >>+<<
    | MHyperFold        -- [>>+<<]
    | MHyperFoldPost    -- [>>+<<]<<
    | MHyperScan        -- [\>>+<<]
    | MHyperScanPost    -- [\>>+<<]<<
    deriving (Show, Enum, Eq, Ord, Typeable, Data, Read)

isQualifiedVar :: Var -> Bool
isQualifiedVar MkVar{ v_package = MkPkg [] } = False
isQualifiedVar _ = True

dropVarPkg :: ByteString -> Var -> Maybe Var
dropVarPkg buf var@MkVar{ v_package = MkPkg ps }
    | (p:_) <- ps, p == buf = Just var{ v_package = MkPkg (tail ps) }
    | otherwise             = Nothing

-- | Package name, composed of multiple parts.
newtype Pkg = MkPkg [ByteString]
    deriving (Eq, Ord, Typeable, Data)

instance Show Pkg where
    show pkg = cast (cast pkg :: ByteString)

instance ((:>:) ByteString) Pkg where
    cast (MkPkg ns) = Str.join (__"::") ns

instance Show Var where
    showsPrec _ var = ('"':) . showsVar var . ('"':)

showsVar :: Var -> String -> String
showsVar MkVar
    { v_sigil   = sig
    , v_twigil  = twi
    , v_package = pkg@(MkPkg ns)
    , v_categ   = cat
    , v_name    = name
    , v_meta    = meta
    } = showsPrec 0 sig . showsPrec 0 twi . showCateg . showPkg . showsMeta meta showName
    where
    showName = ((++) (cast name))
    showCateg = case cat of
        CNil    -> id
        _       -> drop 2 . showsPrec 0 cat . (':':)
    showPkg = if null ns
        then id
        else showsPrec 0 pkg . (\x -> (':':':':x))

showsMeta :: VarMeta -> (String -> String) -> String -> String
showsMeta MNil              f x = f x
showsMeta MFold             f x = ('[':f (']':x))
showsMeta MScan             f x = ('[':'\\':f (']':x))
showsMeta MPre              f x = ('>':'>':f x)
showsMeta MPost             f x = f ('<':'<':x)
showsMeta MHyper            f x = ('>':'>':f ('<':'<':x))
showsMeta MHyperFold        f x = ('[':'>':'>':f ('<':'<':']':x))
showsMeta MHyperFoldPost    f x = ('[':'>':'>':f ('<':'<':']':'<':'<':x))
showsMeta MHyperScan        f x = ('[':'\\':'>':'>':f ('<':'<':']':x))
showsMeta MHyperScanPost    f x = ('[':'\\':'>':'>':f ('<':'<':']':'<':'<':x))

instance ((:>:) String) Var where
    cast var = showsVar var ""

instance ((:>:) String) Pkg where
    cast = cast . (cast :: Pkg -> ByteString)

data VarCateg
    = CNil
    | C_prefix_circumfix_meta_operator
    | C_infix_circumfix_meta_operator
    | C_prefix_postfix_meta_operator
    | C_postfix_prefix_meta_operator
    | C_infix_postfix_meta_operator
    | C_statement_modifier
    | C_statement_control
    | C_scope_declarator
    | C_trait_auxiliary
    | C_trait_verb
    | C_regex_mod_external
    | C_regex_mod_internal
    | C_regex_assertion
    | C_regex_backslash
    | C_regex_metachar
    | C_postcircumfix
    | C_circumfix
    | C_postfix
    | C_infix
    | C_prefix
    | C_quote
    | C_term
    deriving (Show, Enum, Eq, Ord, Typeable, Data, Read)

data VarSigil = SScalar | SArray | SHash | SType | SCode | SRegex | SCodeMulti | SArrayMulti
    deriving (Enum, Eq, Ord, Typeable, Data)

data VarTwigil = TNil | TAttribute | TPrivate | TImplicit | TMagical | TDoc
    | TGlobal -- XXX WRONG!
    deriving (Enum, Eq, Ord, Typeable, Data)

isSigilChar :: Char -> Bool
isSigilChar '$' = True
isSigilChar '@' = True
isSigilChar '%' = True
isSigilChar '&' = True
isSigilChar '<' = True -- XXX wrong
isSigilChar ':' = True
isSigilChar _   = False

instance Show VarSigil where
    showsPrec _ sig = case sig of
        SScalar     -> ('$':)
        SArray      -> ('@':)
        SHash       -> ('%':)
        SCode       -> ('&':)
        SRegex      -> ('<':)
        SType       -> \x -> (':':':':x)
        SCodeMulti  -> \x -> ('&':'&':x)
        SArrayMulti -> \x -> ('@':'@':x)

instance Show VarTwigil where
    showsPrec _ twi = case twi of
        TNil        -> id
        TAttribute  -> ('.':)
        TPrivate    -> ('!':)
        TImplicit   -> ('^':)
        TMagical    -> ('?':)
        TDoc        -> ('=':)
        TGlobal     -> ('*':)

instance ((:>:) VarCateg) ByteString where
    -- XXX slow
    cast buf = case reads ('C':'_':cast buf) of
        ((x, _):_)  -> x
        _           -> internalError $ "Invalid grammatical category: " ++ show buf

instance ((:>:) VarSigil) Char where
    cast '$'    = SScalar
    cast '@'    = SArray
    cast '%'    = SHash
    cast '&'    = SCode
    cast '<'    = SRegex
    cast ':'    = SType
    cast x      = internalError $ "Invalid sigil " ++ show x

instance ((:>:) VarSigil) ByteString where
    cast name
        | name == __"$"     = SScalar
        | name == __"@"     = SArray
        | name == __"%"     = SHash
        | name == __"&"     = SCode
        | name == __"<"     = SRegex
        | name == __":"     = SType
        | name == __"::"    = SType
        | name == __"&&"    = SCodeMulti
        | name == __"@@"    = SArrayMulti
        | otherwise         = internalError $ "Invalid sigil " ++ show name

{-|
Transform an operator name, for example @&infix:\<+\>@ or @&prefix:«[+]»@, 
into its internal name (@&infix:+@ and @&prefix:[+]@ respectively).
-}
instance ((:>:) Var) String where
    cast = cast . (cast :: String -> ByteString)

emptyPkg :: Pkg
emptyPkg = MkPkg []

-- globalPkg :: Pkg
-- globalPkg = MkPkg [__"GLOBAL"]

callerPkg :: Pkg
callerPkg = MkPkg [__"CALLER"]

outerPkg :: Pkg
outerPkg = MkPkg [__"OUTER"]

contextPkg :: Pkg
contextPkg = MkPkg [__"ENV"] -- XXX wrong

nextPkg :: Pkg
nextPkg = MkPkg [__"NEXT"] -- XXX noncanonical

toGlobalVar :: Var -> Var
toGlobalVar var = var{ v_twigil = TGlobal }

isGlobalVar :: Var -> Bool
isGlobalVar MkVar{ v_twigil = TGlobal } = True
isGlobalVar _                           = False

instance ((:>:) Var) ByteString where
    cast x = unsafePerformIO (bufToVar x)

{-# NOINLINE _BufToVar #-}
_BufToVar :: H.StrMap ByteString Var
_BufToVar = unsafePerformIO C.new

bufToVar :: ByteString -> IO Var
bufToVar buf = do
    a' <- C.lookup buf _BufToVar
    maybe (do
        let a = doBufToVar buf
        C.insert buf a _BufToVar
        return a) return a'

doBufToVar :: ByteString -> Var
doBufToVar buf = MkVar
    { v_sigil   = sig'
    , v_twigil  = twi
    , v_package = pkg
    , v_categ   = cat
    , v_meta    = meta
    , v_name    = cast name
    }
    where
    (sig, afterSig) = Str.span isSigilChar buf
    sig' = if Str.null sig then internalError $ "Sigilless var: " ++ show buf else cast sig
    len = Str.length afterSig
    (twi, (pkg, afterPkg))
        | len == 0 = (TNil, (emptyPkg, afterSig))
        | len == 1 = case Str.head afterSig of
            '!' -> (TGlobal, (emptyPkg, afterSig))  -- XXX $! always global - WRONG
            '/' -> (TGlobal, (emptyPkg, afterSig))  -- XXX $/ always global - WRONG
            _   -> (TNil, (emptyPkg, afterSig))
        | otherwise = case Str.head afterSig of
            '.' -> (TAttribute, toPkg afterTwi)
            '^' -> (TImplicit, toPkg afterTwi)
            '?' -> (TMagical, toPkg afterTwi)
            '!' -> (TPrivate, toPkg afterTwi)
            '=' -> (TDoc, toPkg afterTwi)
--          '*' -> (TNil, (globalPkg, Str.tail afterSig))
            '*' -> (TGlobal, toPkg afterTwi)
            '+' -> (TNil, (contextPkg, Str.tail afterSig))
            _   -> (TNil, toPkg (tokenPkg afterSig))
    afterTwi = tokenPkg (Str.tail afterSig)
    toPkg (pkg, rest) = (MkPkg pkg, rest)
    tokenPkg str = case Str.findSubstring (__"::") str of
        Nothing  -> ([], str)
        Just 0   -> tokenPkg (Str.drop 2 str) -- $::x is the same as $x
        Just idx -> let (rest, final) = tokenPkg (Str.drop (idx + 2) str) in
            ((Str.take idx str:rest), final)
    (cat, afterCat)
        | twi == TGlobal =
            -- XXX special case for "$*X::Y::Z".  Currently we encode that
            --     as a twigil of *, and then the name part contains
            case Str.findSubstrings (__"::") afterPkg of
                [] -> tokenizeCategory afterPkg
                xs -> let idx = last xs
                          (c, n) = tokenizeCategory (Str.drop (idx + 2) afterPkg)
                        in (c, Str.take idx afterPkg +++ n)
        | otherwise = tokenizeCategory afterPkg
    (name, meta)
        | C_prefix <- cat, __"\194\171" `Str.isSuffixOf` afterCat
        = (dropEnd 2 afterCat, MPost)
        | C_prefix <- cat, __"<<" `Str.isSuffixOf` afterCat
        = (dropEnd 2 afterCat, MPost)
        | C_postfix <- cat, __"\194\187" `Str.isPrefixOf` afterCat
        = (Str.drop 2 afterCat, MPre)
        | C_postfix <- cat, __">>" `Str.isPrefixOf` afterCat
        = (Str.drop 2 afterCat, MPre)
        | C_infix <- cat
        , __"\194\187" `Str.isPrefixOf` afterCat
        , __"\194\171" `Str.isSuffixOf` afterCat
        = (Str.drop 2 (dropEnd 2 afterCat), MHyper)
        | C_infix <- cat
        , __">>" `Str.isPrefixOf` afterCat
        , __"<<" `Str.isSuffixOf` afterCat 
        = (Str.drop 2 (dropEnd 2 afterCat), MHyper)
        | C_prefix <- cat
        , __"[\\" `Str.isPrefixOf` afterCat
        , ']' <- Str.last afterCat
        = (Str.drop 2 (Str.init afterCat), MScan)
        | C_prefix <- cat
        , '[' <- Str.head afterCat
        , ']' <- Str.last afterCat
        = (Str.tail (Str.init afterCat), MFold)
        -- XXX - MHyperFold, MHyperFoldPost, MHyperScan, MHyperScanPost
        | otherwise
        = (afterCat, MNil)

    tokenizeCategory str = case Str.elemIndex ':' str of
        Just idx -> if isUpper (Str.head str)
            then internalError (show buf)
            else (cast (Str.take idx str), Str.drop (succ idx) str)
        _ -> (CNil, afterPkg)

instance ((:>:) Pkg) ByteString where
    cast = MkPkg . filter (not . Str.null) . Str.splitWith (== ':')

instance ((:>:) Pkg) String where
    cast = cast . (cast :: String -> ByteString)

instance ((:>:) ID) Pkg where
    cast = cast . (cast :: Pkg -> ByteString)

instance ((:>:) Type) ID where
    cast = cast . (cast :: ID -> ByteString)

possiblyFixOperatorName :: Var -> Var
possiblyFixOperatorName var@MkVar{ v_categ = CNil } = var
possiblyFixOperatorName var@MkVar{ v_sigil = sig, v_name = name }
    | sig /= SCode, sig /= SCodeMulti = var
    | __"\194\171" `Str.isPrefixOf` buf, __"\194\187" `Str.isSuffixOf` buf
    = var{ v_name = cast (dropEnd 2 (Str.drop 2 buf)) }
    | __"<<" `Str.isPrefixOf` buf, __">>" `Str.isSuffixOf` buf
    = var{ v_name = cast (dropEnd 2 (Str.drop 2 buf)) }
    | Str.head buf == '<', Str.last buf == '>', buf /= __"<=>"
    = var{ v_name = cast (Str.init (Str.tail buf)) }
    | otherwise
    = var
    where
    buf = cast name

dropEnd :: Int -> ByteString -> ByteString
dropEnd i buf = Str.take (Str.length buf - i) buf

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
countTree (MkClassTree (Node _ [])) = 1
countTree (MkClassTree (Node _ cs)) = 1 + sum (map (countTree . MkClassTree) cs)

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
/bin/bash: line 1: :1: command not found
See 'compareList' for further details.
-}
distanceType :: ClassTree -> Type -> Type -> Int
distanceType (MkClassTree tree) base@(MkType b) target@(MkType t) = 
    IntMap.findWithDefault (compareList l1 l2) (idKey b `shiftL` 16 + idKey t) initCache
--  | not (castOk base target)  = 0
--  | otherwise = compareList l1 l2
    where
    l1 = findList base tree
    l2 = findList target tree
distanceType _ _ _ = error "distanceType: MkType not 'simple'"

initCache :: IntMap.IntMap Int
initCache = IntMap.fromList leaves
    where
    leaves = [ (idKey x `shiftL` 16 + idKey y, cachedLookup x y)
             | x <- initLeaves, y <- initLeaves
             ]
    cachedLookup base target = compareList l1 l2
        where
        l1 = findList base rawTree
        l2 = findList target rawTree

initLeaves :: [ID]
initLeaves = flatten rawTree

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
compareList :: Eq a
            => [a] -- ^ Base type's chain
            -> [a] -- ^ Possibly-derived type's chain
            -> Int
compareList [] _ = -999 -- XXX hack (nonexistent base type?)
compareList _ [] = -999 -- XXX hack (incompatible types)
compareList l1 l2
    | last l1 `elem` l2 =   length(l2 \\ l1) -- compatible types
    | last l2 `elem` l1 = - length(l1 \\ l2) -- anti-compatible types
    | otherwise = compareList l1 (init l2)
{-# SPECIALIZE compareList :: [ID] -> [ID] -> Int #-}
{-# SPECIALIZE compareList :: [Type] -> [Type] -> Int #-}

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
findList :: Eq a
         => a      -- ^ 'Type' to find the inheritance chain of
         -> Tree a -- ^ Class tree to look in
         -> [a]
findList base (Node l cs)
    | base == l                             = [l]
    | Just ls <- find (not . null) found    = l:ls
    | otherwise                             = []
    where
    found = map (findList base) cs
{-# SPECIALIZE findList :: ID -> Tree ID -> [ID] #-}
{-# SPECIALIZE findList :: Type -> Tree Type -> [Type] #-}

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
addNode (MkClassTree (Node obj [Node any (Node item ns:rest), junc])) typ =
    MkClassTree (Node obj [Node any (Node item ((Node typ []):ns):rest), junc])
addNode _ _ = error "malformed tree"

{-|
Default class tree, containing all built-in types.
-}
initTree :: ClassTree
initTree = MkClassTree (fmap MkType rawTree)

rawTree :: Tree ID
rawTree = fmap cast $! Node "Object"
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
                , Node "Regex" []
                , Node "Signature" []
                , Node "Capture"
                    [ Node "Match" []
                    ]
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
        , Node "Pair"
            [ Node "Pair::HashSlice" []
            ]
        ]
    , Node "Junction" [] ]

