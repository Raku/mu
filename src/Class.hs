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
    , clsSuper      :: MetaClass
    , clsSubClasses :: Set MetaClass
    , clsProperties :: FiniteMap Label (Visibility, MetaProperty)
    , clsMethods    :: FiniteMap Label (Visibility, MetaMethod)
    , clsAssocs     :: FiniteMap Label MetaAssoc
    , clsRevAssocs  :: FiniteMap Label MetaAssoc
    }

{-
    Rules of these collections;

    ∃ MetaClass A, B : A.clsSuper = B ↔ A ∈ B.clsSupClasses

    ∃ MetaClass A, MetaAssoc B : A.clsAssocs ∋ B ↔ B.assocSource = A
    ∃ MetaClass A, MetaAssoc B : A.clsRevAssocs ∋ B ↔ B.assocTarget = A

-}

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

{-
    these classes represent the Perl 6 Class model and/or type system

    So far, there exists only this pseudo-code :)

    ∀ initTree Node N ∃ MetaClass M : M.clsName = N

    Note: in the below expression, N₁ ∋ N₂ means (N₂ is a direct
    child member of N₁ within the tree it exists in)

    ∃ initTree Node N₁, N₂, MetaClass M₁, M₂ 
      : N₁ ∋ N₂ ∧ N₁ = M₁.clsName ∧ N₂ = M₂.clsName
      → M₁.subClasses ∋ M₂ 

    -- 

  Note: what follows might all be kack, and is written by someone who
        hasn't read http://xrl.us/tapl, which is not ideal.  Maybe
        someone who has will come along later and fix this.

  Package := MetaClass where clsName = "Package"

{-
  PkgIsGlobal is not quite right - a package is global if it exists
  in the global package namespace.  Packages either need to
  know their "own" namespace for $?PACKAGE to work (perhaps...), or
  have a back-reference to the namespace they exist in that has a
  String category that is the name, or something like that.  consider
  this a FIXME :-)
-}

something like that.
  Package.clsProperties =
	{ pkgName = MetaProperty { type = Symbol } 
	  pkgIsGlobal = MetaProperty { type = Bool  }
	}

  Package.clsAssocs =
	{ pkgSubPackages = MetaAssoc { targetClass = Package }
	}
    
  MetaClass where clsName = "Class"
     clsProperties =
	[ 


	]
     
     

-}

