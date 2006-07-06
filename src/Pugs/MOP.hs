
import Pugs.MOP.Instances

pugsMetaModelVersion :: [Int]
pugsMetaModelVersion = [0, 0, 1]

{- data types for Package, Module, Class, Grammar, and Role -}

data Package = MkPackage
    { packageName     :: Ident
    , packageParent   :: Maybe Package
    }

data Module = MkModule
    { moduleVersion   :: Data.Version
    , moduleAuthority :: Network.URI
    }

data Class = MkClass
    { classModule              :: Module
    , classSuperClasses        :: [Class]
    , classRuntimeSuperClasses :: Eval [Class] -- list of runtime-added superclasses
    , classMethodTable         :: Map Ident Code
    , classRuntimeMethodtable  :: Eval (Map Ident Code)
    , classRuntimeSlots        :: Eval (Map Ident (TVar Val))
    }

newtype Grammar = MkGrammar { grammarClass :: Class }

newtype Role    = MkRole    { grammarRole  :: Class }


class Boxable a where
    meta  :: a -> Class
    getId :: a -> Val

-- These will be derived by DrIFT eventually :)
instance Boxable VInt where
    meta  _ = mkClass "VInt"
    getId _ = 6147 -- unique per built-in Val type

{- Bootstrap initial metaobject instances -}
packageMeta, packageRoot :: Package
packageRoot = MkPackage "GLOBAL" Nothing
packageMeta = MkPackage "Class" (Just packageRoot)

moduleMeta :: Module
moduleMeta = MkModule
    { moduleVersion   = pugsMetaModelVersion []
    , moduleAuthority = parseURI "urn:Pugs"
    , modelePackage   = packageMeta
    }

classMeta, classObject :: Class
classMeta = MkClass
    { classModule              = moduleMeta
    , classSuperClasses        = [classObject]
    , classRuntimeSuperClasses = return []
    , classMethodTable         = -- stevan xx Inf
    , classRuntimeMethodtable  = return empty
    , classRuntimeSlots        = return -- stevan
    }

classObject = MkClass
    { classModule              = moduleObject
    , classSuperClasses        = []
    , classRuntimeSuperClasses = return []
    , classMethodTable         = --stevan
    , classRuntimeMethodtable  = return empty
    , classRuntimeSlots        = return --punt
    }


{-
 - HashCons cache, to be spelled out via DrIFT to avoid enumeration typos
defaultClassMap :: Map String Class
defaultClassMap = fromList [ (x, mkClass x) | x <- initialClasses ]
    where initialClasses =
        [ "VInt"
        , ...
        ]

getClass :: String -> Class
getClass = Map.lookup defaultClassMap
-}

getClass :: String -> Class
getClass = mkClass

mkClass :: String -> Class
mkClass name = MkClass
    { classModule = mkMod'
    , classSuperClasses = classMeta
    , classRuntimeSuperClasses = return []
    , classMethodTable = -- ??
    , classRuntimeMethodtable = return empty
    , classRuntimeSlots = return -- punt
    }
    where
    mkMod' = moduleMeta{ modulePackage = mkPkg' }
    mkPkg' = MkPackage{ packageName = name, packageParent = Just packageMeta }

mkGrammar :: String -> Grammar
mkGrammar = MkGrammar . mkClass

mkRole :: String -> Role
mkRole = MkRole . mkClass


