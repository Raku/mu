{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.MetaModel where
import PIL.Container
import PIL.Internals
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

type ObjectData  = IntMap Container
data Object = MkObject
    { objId     :: Id
    , objClass  :: Class
    , objData   :: ObjectData
    }
    deriving (Eq, Ord, Show, Typeable)

type ClassFields = Map Name Int
data Class = MkClass
    { clsMeta   :: Object
    , clsFields :: ClassFields
    }
    deriving (Eq, Ord, Show, Typeable)

-- Perl6::MetaClass - &new - Blank state for demiurge below
gnosis :: Name -> Map Name Container
gnosis name = Map.mapKeys MkName . Map.fromList $
    [ ("$.name",                constScalar name)
    , ("$.version",             constScalar undef) -- 0.0.1?
    , ("$.authority",           constScalar undef)
    , ("@:superclasses",        constArray [])     -- appendArray?
    , ("%:private",             methodAttrHash)
    , ("%:class_definition",    methodAttrHash)
    , ("%:class_data",          methodAttrHash)
    ]
    where
    methodAttrHash = constHash
        [ "methods",    mutableHash []
        , "attributes", mutableHash []
        ]

-- Perl6::MetaClass - &_build_meta line 1 - First metaclass by parthenogenesis
demiurge :: Object
demiurge = MkObject{ objId = undefined, objClass = cls, objData = dat }
    where
    (fld, dat) = splitMap (gnosis $ MkName undef) -- no $.name
    cls = MkClass{ clsMeta = demiurge, clsFields = fld }

-- Perl6::Object - First call to &Perl6::MetaModel::class
pneuma :: Object
pneuma = MkObject{ objId = undefined, objClass = cls, objData = logos }
    where
    (fld, dat) = splitMap (gnosis $ MkName "Perl6::Object")
    cls = MkClass{ clsMeta = demiurge{ objData = dat }, clsFields = fld }

-- Class definition for pneuma above
logos :: ObjectData
logos = undefined

-- Bootstrapped MetaClass - suitable for applying to environment
theos :: Object
theos = metamorph demiurge

-- &Perl6::MetaClass::_build_meta - Turn demiurge to a fully powered theos
metamorph :: Object -> Object
metamorph = undefined

-- The "BOOTSTRAPPING" section in Perl6::MetaModel
genesis :: STM ()
genesis = dispatch 
    (metaOf theos)
    (MkName "superclasses")
    (constArray [metaOf pneuma])
    where
    metaOf = clsMeta . objClass

-- &Perl6::MetaModel::dispatch - divided into metaclass vs normal dispatch
dispatch :: Object -> Name -> t -> STM ()
dispatch = undefined

-------------------------------------

splitMap = undefined
undef = undefined
constScalar = undefined
constArray = undefined
constHash = undefined
mutableHash = undefined
