{-# OPTIONS -fglasgow-exts -cpp #-}

{-
    Class meta-model.  (object meta-meta-model)

    Learn now the lore of Living Creatures!
    First name the four, the free peoples:
    Eldest of all, the elf-children;
    Dwarf the delver, dark are his houses;
    Ent the earthborn, old as mountains;
    Man the mortal, master of horses...
-}

module Class where
import AST
import Internals

{-
    instances of these objects represent the Perl 6 Class Model, ie
    with names like "Class", "Role", "Trait", etc.
-}

data MetaClass = MetaClass
    { clsName       :: Label
    , clsParent     :: MetaClass
    , clsChildren   :: Set MetaClass
    , clsProperties :: FiniteMap Label (Visibility, MetaProperty)
    , clsMethods    :: FiniteMap Label (Visibility, MetaMethod)
    , clsAssocs     :: FiniteMap Label MetaAssoc
    , clsRevAssocs  :: FiniteMap Label MetaAssoc
    }

data MetaMethod = MetaMethod
    { methodParams  :: Params
    , methodInvoke  :: [Val] -> Eval Val
    }

data MetaProperty = MetaProperty
    { propType          :: Type
    , propDefault       :: Eval Val
    }

data MetaAssoc = MetaAssoc
    { assocSource       :: MetaClass
    , assocTarget       :: MetaClass
    , assocSourceRange  :: Range -- True = Inf, False = 1
    , assocTargetRange  :: Range -- True = Inf, False = 1
    , assocCategory     :: Category
    , assocIsComposite  :: Bool     -- if you kill this, its children makes no sense to live either
    }

data Visibility = Public | Private

type Label = String

type Range = (Multi, Multi)

data Multi = Zero | One | Many

data Category = Unordered | Ordered | Keyed

data Type = Int | Str

{-
refAssoc    = (Inf, 1)
refForeign  = (1, Inf)
refSet      = (Inf, Inf)
refSlave    = (1, 1)
-}
