{-# OPTIONS -fglasgow-exts -cpp #-}

{-
    Class meta-model.

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

data Class = Class
    { clsName       :: Label
    , clsParent     :: Class
    , clsChildren   :: Set Class
    , clsProperties :: FiniteMap Label (Visibility, Property)
    , clsMethods    :: FiniteMap Label (Visibility, Method)
    , clsAssocs     :: FiniteMap Label Assoc
    , clsRevAssocs  :: FiniteMap Label Assoc
    }

data Method = Method
    { methodParams  :: Params
    , methodInvoke  :: [Val] -> Eval Val
    }

data Property = Property
    { propType          :: Type
    , propDefault       :: Eval Val
    }

data Assoc = Assoc
    { assocSource       :: Class
    , assocTarget       :: Class
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
