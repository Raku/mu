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


{-
  PkgIsGlobal is not quite right - a package is global if it exists
  in the global package namespace.  Packages either need to
  know their "own" namespace for $?PACKAGE to work (perhaps...), or
  have a back-reference to the namespace they exist in that has a
  String category that is the name, or something like that.  consider
  this a FIXME :-)
 -}

  Package := MetaClass where clsName = "Package"
  Package.clsProperties =
	{ pkgName = MetaProperty { type = Symbol } 
	, pkgIsGlobal = MetaProperty { type = Bool  }
	, pkgStash = MetaProperty { type = FiniteMap (sigil, Symbol) Object }
	}

  Package.clsAssocs =
	{ pkgSubPackages = MetaAssoc { assocTarget = Package,
				       assocComposite = true }
	}

{-
  Traits - just what do we know about them?  They're mentioned in S02,
           S04, etc as applying to Packages, Blocks, etc.

           Perhaps *all* objects should be able to have generic
           "Traits" in the Meta-Model ?

           Or are traits just the word we use to mean a property of
           something in the MetaModel?  In the context of packages,
           they seem to be more generic than that.  This is why I have
           made this specifically a PkgTrait class
 -}
  PkgTrait := MetaClass where clsName = "PkgTrait"
    
  Module := MetaClass where clsName = "Module"
  Module.clsProperties =
	{ modVersion = MetaProperty { type = Version }
	, modAuthorizer = MetaProperty { type = String }
	}

  Module.clsMethods =
	{ modName = MetaMethod
	      { methodInvoke = ( self.pkgName
                               ~ "-" ~ self.modVersion
			       ~ "-" ~ self.modAuthorizer ) }
	}

  Module.clsAssocs =
	{ modTraits = MetaAssoc { targetClass = PkgTrait,
				  assocCategory = Keyed,
				  assocComposite = true,
				 }
	}
  

-}

