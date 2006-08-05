{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Pugs.MOP where

import Data.Map
import qualified Network.URI
import qualified Data.ByteString.Char8 as Str
import Data.Version

import Pugs.AST.CapInternals

type Str = Str.ByteString

-- import Pugs.MOP.Instances

pugsMetaModelVersion :: [Int]
pugsMetaModelVersion = [0, 0, 1]

{-| data types for Package, Module, Class, Grammar, and Role -}


class Boxable a where
    meta  :: a -> Class
    getId :: a -> Val

-- These will be derived by DrIFT eventually :)
instance Boxable Int where
    meta  _ = mkClass "NativeInt"
    --        vvvvvvvvvvvvvvvvvvvvvvvv we'd better come up with some convenience funcs here
    getId _ = VNative $ NInt $ IFinite 6147 -- unique per built-in Val type

{- Bootstrap initial metaobject instances -}
packageObject, packageMeta, packageRoot :: Package
packageRoot   = MkPackage (Str.pack "GLOBAL") Nothing
packageMeta   = MkPackage (Str.pack "Class")  (Just packageRoot)
packageObject = MkPackage (Str.pack "Object") (Just packageRoot)

moduleMeta, moduleObject :: Module
moduleMeta = MkModule
    { m_version   = Version pugsMetaModelVersion []
    , m_authority = Network.URI.parseURI "urn:Pugs"
    , m_package   = Just packageMeta
    }

moduleObject = moduleMeta{ m_package = Just packageObject }

classMeta, classObject :: Class
classMeta = MkClass
    { c_module              = Just moduleMeta
    , c_superClasses        = [classObject]
    , c_runtimeSuperClasses = Eval [] -- return []
    , c_methodTable         = empty
    , c_runtimeMethodtable  = Eval empty -- return empty
    , c_runtimeSlots        = Eval empty -- return empty -- stevan
    }

classObject = MkClass
    { c_module              = Just moduleObject
    , c_superClasses        = []
    , c_runtimeSuperClasses = Eval [] -- return []
    , c_methodTable         = empty
    , c_runtimeMethodtable  = Eval empty -- return empty
    , c_runtimeSlots        = Eval empty -- return empty --punt
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
    , c_superClasses = [classMeta]
    , c_runtimeSuperClasses = Eval [] -- return []
    , c_methodTable = empty
    , c_runtimeMethodtable = Eval empty -- return empty
    , c_runtimeSlots = Eval empty -- return empty -- punt
    }
    where
    mkMod' = Just moduleMeta{ m_package = mkPkg' }
    mkPkg' = Just MkPackage{ p_name = Str.pack name, p_parent = Just packageMeta }

mkGrammar :: String -> Grammar
mkGrammar = MkGrammar . mkClass

mkRole :: String -> Role
mkRole = MkRole . mkClass


