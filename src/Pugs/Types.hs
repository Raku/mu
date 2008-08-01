{-# OPTIONS_GHC -fglasgow-exts -fno-warn-deprecations -fno-warn-orphans -funbox-strict-fields -fallow-overlapping-instances -fparr #-}
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
    ClassTree, initTree,

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
import qualified Data.Map as Map
import qualified Data.HashTable as H
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Char8 as Buf -- Intentionally not UTF8!
import qualified Data.ByteString as B (findSubstring)

data Type
    = MkType !ID            -- ^ A regular type
    | TypeOr  !Type !Type   -- ^ The disjunction (|) of two types
    | TypeAnd !Type !Type   -- ^ The conjunction (&) of two types
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
    { v_name    :: !ID
    , v_sigil   :: !VarSigil
    , v_twigil  :: !VarTwigil
    , v_categ   :: !VarCateg
    , v_package :: !Pkg
    , v_meta    :: !VarMeta
    , v_longname:: !ID
    }
    deriving (Eq, Ord, Typeable, Data)

-- | a dummy scalar, used for example as the invocant
-- in the signature :( $ : $x, $y ).
varNullScalar :: Var
varNullScalar = MkVar
    { v_name    = nullID
    , v_longname= nullID
    , v_sigil   = SScalar
    , v_twigil  = TNil
    , v_categ   = CNil
    , v_package = MkPkg []
    , v_meta    = MNil
    }

-- | the topical variable $_
varTopic :: Var
varTopic = cast "$_"

data VarMeta
    = MNil
    | MFold             -- [+]
    | MScan             -- [\+]
--  | MFoldPost         -- [+]<<
--  | MScanPost         -- [\+]<<
    | MPre              -- >>+
    | MPost             -- +<<
    | MHyper            -- >>+<<
    | MHyperFold        -- [>>+<<]
--  | MHyperFoldPost    -- [>>+<<]<<
    | MHyperScan        -- [\>>+<<]
--  | MHyperScanPost    -- [\>>+<<]<<
    deriving (Show, Enum, Eq, Ord, Typeable, Data, Read)

isLexicalVar :: Var -> Bool
isLexicalVar MkVar{ v_package = MkPkg [], v_twigil = twi } = case twi of
    TGlobal -> False
    TDoc    -> False    -- XXX noncanonical
    _       -> True
isLexicalVar _ = False

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
    cast (MkPkg ns) = Buf.intercalate (__"::") ns

instance Show Var where
    show var = show (cast var :: ByteString)

varToBuf :: Var -> ByteString
varToBuf MkVar
    { v_sigil   = sig
    , v_twigil  = twi
    , v_package = (MkPkg ns)
    , v_categ   = cat
    , v_name    = name
    , v_longname= longname
    , v_meta    = meta
    } = Buf.concat . (cast sig:) . (cast twi:) . doPkg ns . doCat $ doMeta
    where
    n = cast name
    l = cast longname
    doPkg []     = id
    doPkg (x:xs) = (x:) . (__"::":) . doPkg xs
    doCat = case cat of
        CNil    -> id
        _       -> (cast cat:) . (__":":)
    doMeta = case meta of
        MNil        -> [n, l]
        MFold       -> [__"[", n, l, __"]"]
        MScan       -> [__"[\\", n, l, __"]"]
        MPre        -> [__">>", n, l]
        MPost       -> [n, l, __"<<"]
        MHyper      -> [__">>", n, l, __"<<"]
        MHyperFold  -> [__"[>>", n, l, __"<<]"]
        MHyperScan  -> [__"[\\>>", n, l, __"<<]"]

showsVar :: Var -> String -> String
showsVar var = (show var ++)

showsMeta :: VarMeta -> (String -> String) -> String -> String
showsMeta MNil              f x = f x
showsMeta MFold             f x = ('[':f (']':x))
showsMeta MScan             f x = ('[':'\\':f (']':x))
--showsMeta MFoldPost         f x = ('[':f (']':'<':'<':x))
--showsMeta MScanPost         f x = ('[':'\\':f (']':'<':'<':x))
showsMeta MPre              f x = ('>':'>':f x)
showsMeta MPost             f x = f ('<':'<':x)
showsMeta MHyper            f x = ('>':'>':f ('<':'<':x))
showsMeta MHyperFold        f x = ('[':'>':'>':f ('<':'<':']':x))
--showsMeta MHyperFoldPost    f x = ('[':'>':'>':f ('<':'<':']':'<':'<':x))
showsMeta MHyperScan        f x = ('[':'\\':'>':'>':f ('<':'<':']':x))
--showsMeta MHyperScanPost    f x = ('[':'\\':'>':'>':f ('<':'<':']':'<':'<':x))

instance ((:>:) ByteString) Var where
    cast var = varToBuf var

instance ((:>:) String) Var where
    cast var = cast (varToBuf var)

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
    deriving (Show, Enum, Eq, Ord, Bounded, Typeable, Data, Read)

data VarSigil = SScalar | SArray | SHash | SType | SCode | SRegex | SArrayMulti
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

instance ((:>:) ByteString) VarSigil where
    cast x = case x of
        SScalar     -> __"$"
        SArray      -> __"@"
        SHash       -> __"%"
        SCode       -> __"&"
        SRegex      -> __"<"
        SType       -> __"::"
        SArrayMulti -> __"@@"

instance ((:>:) ByteString) VarTwigil where
    cast x = case x of
        TNil        -> Buf.empty
        TAttribute  -> __"."
        TPrivate    -> __"!"
        TImplicit   -> __"^"
        TMagical    -> __"?"
        TDoc        -> __"="
        TGlobal     -> __"*"

instance Show VarSigil where
    showsPrec _ sig = case sig of
        SScalar     -> ('$':)
        SArray      -> ('@':)
        SHash       -> ('%':)
        SCode       -> ('&':)
        SRegex      -> ('<':)
        SType       -> \x -> (':':':':x)
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

-- Cached Categ->ByteString mappings.
catBufMap :: [:ByteString:]
catBufMap = mapP (_cast . drop 2 . show) [:minBound..(maxBound :: VarCateg):]

-- Cached ByteString->Categ mappings.
bufCatMap :: Map ByteString VarCateg
bufCatMap = Map.fromList (fromP catBufMap `zip` [minBound..(maxBound :: VarCateg)])

instance ((:>:) ByteString) VarCateg where
    cast categ = catBufMap !: fromEnum categ

instance ((:>:) (Maybe VarCateg)) ByteString where
    cast buf = Map.lookup buf bufCatMap

instance ((:>:) VarCateg) ByteString where
    cast buf = case Map.lookup buf bufCatMap of
        Just x  -> x
        _       -> internalError $ "Invalid grammatical category: " ++ show buf

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
        | name == __"&&"    = SCode -- XXX
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

mainPkg :: Pkg
mainPkg = MkPkg [__"Main"]

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
isGlobalVar MkVar{ v_twigil = TDoc }    = True -- XXX noncanonical
isGlobalVar _                           = False

instance ((:>:) Var) ByteString where
    cast x = unsafePerformIO (bufToVar x)

{-# NOINLINE _BufToVar #-}
_BufToVar :: H.HashTable ByteString Var
_BufToVar = unsafePerformIO hashNew

bufToVar :: ByteString -> IO Var
bufToVar buf = do
    a' <- H.lookup _BufToVar buf
    maybe (do
        let a = doBufToVar buf
        H.insert _BufToVar buf a
        return a) return a'

doBufToVar :: ByteString -> Var
doBufToVar buf = MkVar
    { v_sigil   = sig'
    , v_twigil  = twi
    , v_package = pkg
    , v_categ   = cat
    , v_meta    = meta
    , v_name    = name
    , v_longname= longname
    }
    where
    (sig, afterSig) = Buf.span isSigilChar buf
    sig' = if Buf.null sig then internalError $ "Sigilless var: " ++ show buf else cast sig
    len = Buf.length afterSig
    (twi, (pkg, (cat, afterCat)))
        | len == 0 = (TNil, (emptyPkg, (CNil, afterSig)))
--      | len <= 1 = (TNil, (emptyPkg, (CNil, afterSig)))
        | len == 1 = case Buf.head afterSig of
            '!' -> (TGlobal, (emptyPkg, (CNil, afterSig)))  -- XXX $! always global - WRONG
            '/' -> (TGlobal, (emptyPkg, (CNil, afterSig)))  -- XXX $/ always global - WRONG
            _   -> (TNil, (emptyPkg, (CNil, afterSig)))
        | otherwise = case Buf.head afterSig of
            '.' -> (TAttribute, toPkg afterTwi)
            '^' -> (TImplicit, toPkg afterTwi)
            '?' -> (TMagical, toPkg afterTwi)
            '!' -> (TPrivate, toPkg afterTwi)
            '=' -> (TDoc, toPkg afterTwi)
--          '*' -> (TNil, (globalPkg, Buf.tail afterSig))
            '*' -> (TGlobal, toPkg afterTwi)
            '+' -> (TNil, (contextPkg, snd afterTwi))
            _   -> (TNil, toPkg (tokenPkg afterSig))
    afterTwi = tokenPkg (Buf.tail afterSig)
    toPkg (pg, rest) = (MkPkg pg, rest)
    tokenPkg :: ByteString -> ([ByteString], (VarCateg, ByteString))
    tokenPkg str = case Buf.elemIndex ':' str of
        Just idx1 -> case B.findSubstring (__":(") str of
            Just idxSig | idx1 == idxSig -> ([], (CNil, str))
            _ -> case B.findSubstring (__"::") str of
                Nothing  -> ([], (cast (Buf.take idx1 str), Buf.drop (succ idx1) str))
                Just 0   -> tokenPkg (Buf.drop 2 str) -- '$::x' is the same as $x
                Just idx
                    | idx == idx1 -> case cast (Buf.take idx1 str) of
                        -- &infix::= should parse as infix:<:=>, not infix::<=>
                        Just ct -> ([], (ct, Buf.drop (succ idx1) str))
                        -- &Infix::= should parse as Infix::<=>, not Infix:<:=>
                        _        -> let (rest, final) = tokenPkg (Buf.drop (idx + 2) str) in
                            ((Buf.take idx str:rest), final)
                    | otherwise -> ([], (cast (Buf.take idx1 str), Buf.drop (succ idx1) str))
        _ -> ([], (CNil, str))
    (name, longname) = case B.findSubstring (__":(") fullname of
        Just idx -> (cast (Buf.take idx fullname), cast (Buf.drop idx fullname))
        _        -> (cast fullname, nullID)
    (fullname, meta)
        | C_postfix <- cat, __"\187" `Buf.isPrefixOf` afterCat
        = (Buf.drop 2 afterCat, MPre)
        | C_postfix <- cat, __">>" `Buf.isPrefixOf` afterCat
        = (Buf.drop 2 afterCat, MPre)
        | C_infix <- cat
        , __"\187" `Buf.isPrefixOf` afterCat
        , __"\171" `Buf.isSuffixOf` afterCat
        = (Buf.drop 2 (dropEnd 2 afterCat), MHyper)
        | C_infix <- cat
        , __">>" `Buf.isPrefixOf` afterCat
        , __"<<" `Buf.isSuffixOf` afterCat
        = (Buf.drop 2 (dropEnd 2 afterCat), MHyper)
        | C_prefix <- cat
        , __"[\\" `Buf.isPrefixOf` afterCat
        , ']' <- Buf.last afterCat
        = case Buf.drop 2 (Buf.init afterCat) of
            maybeHyper | __">>" `Buf.isPrefixOf` maybeHyper
                       , __"<<" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperScan)
            maybeHyper | __"\187" `Buf.isPrefixOf` maybeHyper
                       , __"\171" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperScan)
            other -> (other, MScan)
        | C_prefix <- cat
        , '[' <- Buf.head afterCat
        , ']' <- Buf.last afterCat
        = case Buf.tail (Buf.init afterCat) of
            maybeHyper | __">>" `Buf.isPrefixOf` maybeHyper
                       , __"<<" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperFold)
            maybeHyper | __"\187" `Buf.isPrefixOf` maybeHyper
                       , __"\171" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperFold)
            other -> (other, MFold)
        -- XXX - massive cut-n-paste!
        {-
        | C_prefix <- cat
        , __"[\\" `Buf.isPrefixOf` afterCat
        , __"]\171" `Buf.isSuffixOf` afterCat || __"]<<" `Buf.isSuffixOf` afterCat
        = case Buf.drop 2 (dropEnd 3 afterCat) of
            maybeHyper | __">>" `Buf.isPrefixOf` maybeHyper
                       , __"<<" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperScanPost)
            maybeHyper | __"\187" `Buf.isPrefixOf` maybeHyper
                       , __"\171" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperScanPost)
            other -> (other, MScanPost)
        | C_prefix <- cat
        , '[' <- Buf.head afterCat
        , __"]\171" `Buf.isSuffixOf` afterCat || __"]<<" `Buf.isSuffixOf` afterCat
        = case Buf.tail (dropEnd 3 afterCat) of
            maybeHyper | __">>" `Buf.isPrefixOf` maybeHyper
                       , __"<<" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperFoldPost)
            maybeHyper | __"\187" `Buf.isPrefixOf` maybeHyper
                       , __"\171" `Buf.isSuffixOf` maybeHyper
                -> (Buf.drop 2 (dropEnd 2 maybeHyper), MHyperFoldPost)
            other -> (other, MFoldPost)
        -}
        | C_prefix <- cat, __"\171" `Buf.isSuffixOf` afterCat
        = (dropEnd 2 afterCat, MPost)
        | C_prefix <- cat, __"<<" `Buf.isSuffixOf` afterCat
        = (dropEnd 2 afterCat, MPost)
        | otherwise
        = (afterCat, MNil)

instance ((:>:) Pkg) ByteString where
    cast = MkPkg . filter (not . Buf.null) . Buf.splitWith (== ':')

instance ((:>:) Pkg) String where
    cast = cast . (cast :: String -> ByteString)

instance ((:>:) ID) Pkg where
    cast = cast . (cast :: Pkg -> ByteString)

instance ((:>:) Type) ID where
    cast = cast . (cast :: ID -> ByteString)

possiblyFixOperatorName :: Var -> Var
possiblyFixOperatorName var@MkVar{ v_categ = CNil } = var
possiblyFixOperatorName var@MkVar{ v_sigil = SCode, v_name = name }
    | __"\171" `Buf.isPrefixOf` buf, __"\187" `Buf.isSuffixOf` buf
    = var{ v_name = cast (dropEnd 2 (Buf.drop 2 buf)) }
    | __"<<" `Buf.isPrefixOf` buf, __">>" `Buf.isSuffixOf` buf
    = var{ v_name = cast (dropEnd 2 (Buf.drop 2 buf)) }
    | Buf.head buf == '<', Buf.last buf == '>', buf /= __"<=>"
    = var{ v_name = cast (Buf.init (Buf.tail buf)) }
    | otherwise
    = var
    where
    buf = cast name
possiblyFixOperatorName var = var

dropEnd :: Int -> ByteString -> ByteString
dropEnd i buf = Buf.take (Buf.length buf - i) buf

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
    compare x y = compare (addressOf x) (addressOf y)
instance Eq (TMVar a) where
    x == y = addressOf x == addressOf y
instance Show (TMVar a) where
    show = showAddressOf "tmvar"

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
deltaType :: Type      -- ^ Base type
          -> Type      -- ^ Possibly-derived type
          -> Int
deltaType = junctivate min max $ \base target ->
    let distance = distanceType base target in
    if distance < 0
        then initTreeCount - distance
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
           -> (Type -> Type -> t)
                            -- ^ Function that will actually perform the
                            --     comparison (on non-junctive types)
           -> Type          -- ^ First type to compare
           -> Type          -- ^ Second type to compare
           -> t
junctivate ors ands f base target
    | TypeOr t1 t2 <- target
    = redo base t1 `ors` redo base t2
    | TypeOr b1 b2 <- base
    = redo b1 target `ors` redo b2 target
    | TypeAnd t1 t2 <- target
    = redo base t1 `ands` redo base t2
    | TypeAnd b1 b2 <- base
    = redo b1 target `ands` redo b2 target
    | otherwise
    = f base target
    where
      redo = junctivate ors ands f

-- When saying Int.isa(Scalar), Scalar is the base, Int is the target
{-|
A more convenient version of 'isaType\'' that automatically converts the base
type string into an actual 'Type' value.
-}
isaType :: String    -- ^ Base type
        -> Type      -- ^ Possibly-derived type
        -> Bool
isaType base target = isaType' (mkType base) target

{-|
Return true if the second type (the \'target\') is derived-from or equal-to the
first type (the \'base\'), in the context of the given class tree.

This function will autothread over junctive types.
-}
isaType' :: Type      -- ^ Base type
         -> Type      -- ^ Possibly-derived type
         -> Bool
isaType' = junctivate (||) (&&) $ \base target ->
    distanceType base target >= 0

{-|
Compute the \'distance\' between two types by applying 'findList' to each of
/bin/bash: line 1: :1: command not found
See 'compareList' for further details.
-}
distanceType :: Type -> Type -> Int
distanceType base@(MkType b) target@(MkType t) =
    IntMap.findWithDefault (distanceType base' target') (bk `shiftL` 16 + tk) initCache
--  | not (castOk base target)  = 0
--  | otherwise = compareList l1 l2
    where
    bk      = idKey b
    tk      = idKey t
    base'   = if IntSet.member bk initKeySet then base else anonType1
    target' = if IntSet.member tk initKeySet then target else anonType2
distanceType _ _ = error "distanceType: MkType not 'simple'"

anonType1, anonType2 :: Type
anonType1 = mkType "\1"
anonType2 = mkType "\2"

initKeySet :: IntSet.IntSet
initKeySet = IntSet.fromList (map idKey initLeaves)

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
Default class tree, containing all built-in types.
-}
initTree :: ClassTree
initTree = MkClassTree (fmap MkType rawTree)

initTreeCount :: Int
initTreeCount = countTree initTree

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
                , Node "IO" []
                , Node "IO::Dir" []
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
                        [ Node "Loop" [] ]
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
    , Node "Junction" []
    , Node "\1" []
    , Node "\2" []
    ]

