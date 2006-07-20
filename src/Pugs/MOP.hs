module Pugs.MOP where

import qualified Data.Version
import qualified Network.URI

-- import Pugs.MOP.Instances

pugsMetaModelVersion :: [Int]
pugsMetaModelVersion = [0, 0, 1]

{-| data types for Package, Module, Class, Grammar, and Role -}

data Package = MkPackage
    { p_name     :: Ident
    , p_parent   :: Maybe Package
    }

data Module = MkModule
    { m_version   :: Data.Version
    , m_authority :: Network.URI
    , m_package   :: Maybe Package
    }

data Class = MkClass
    { c_module              :: Maybe Module
    , c_superClasses        :: [Class]
    , c_runtimeSuperClasses :: Eval [Class] -- list of runtime-added superclasses
    , c_methodTable         :: Map Ident Code
    , c_runtimeMethodtable  :: Eval (Map Ident Code)
    , c_runtimeSlots        :: Eval (Map Ident (TVar Val))
    }

newtype Grammar = MkGrammar { g_class :: Class }

newtype Role    = MkRole    { r_role  :: Class }


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
    { m_version   = pugsMetaModelVersion []
    , m_authority = parseURI "urn:Pugs"
    , m_package   = packageMeta
    }

classMeta, classObject :: Class
classMeta = MkClass
    { c_module              = moduleMeta
    , c_superClasses        = [classObject]
    , c_runtimeSuperClasses = return []
    , c_methodTable         = error "XXX stevan" -- stevan xx Inf
    , c_runtimeMethodtable  = return empty
    , c_runtimeSlots        = return -- stevan
    }

classObject = MkClass
    { c_module              = moduleObject
    , c_superClasses        = []
    , c_runtimeSuperClasses = return []
    , c_methodTable         = error "XXX stevan" --stevan
    , c_runtimeMethodtable  = return empty
    , c_runtimeSlots        = return --punt
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
    { c_module = mkMod'
    , c_superClasses = classMeta
    , c_runtimeSuperClasses = return []
    , c_methodTable = error "XXX stevan" -- ??
    , c_runtimeMethodtable = return empty
    , c_runtimeSlots = return -- punt
    }
    where
    mkMod' = moduleMeta{ m_package = mkPkg' }
    mkPkg' = MkPackage{ p_name = name, p_parent = Just packageMeta }

mkGrammar :: String -> Grammar
mkGrammar = MkGrammar . mkClass

mkRole :: String -> Role
mkRole = MkRole . mkClass


