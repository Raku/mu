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

  DEFINITIONS
  -----------

    Get these right, or you will burn forever in Meta-Meta-Hell.

  Haskell    S12 term    Access from Perl as
  -------    --------    -------------------
   MetaClass    -         MyClass.meta.meta
   Class      MetaClass   MyClass.meta
   ?          Class       MyClass

    Looks like we still need an angel to figure this all out!  :-)

-}

data MetaClass = MetaClass
    { clsName       :: Label
    , clsSuper      :: MetaClass
    , clsSubClasses :: Set MetaClass
    , clsProperties :: Map Label (Visibility, MetaProperty)
    , clsMethods    :: Map Label (Visibility, MetaMethod)
    --, clsAssocs     :: Map Label MetaAssoc
    --, clsRevAssocs  :: Map Label MetaAssoc
    , clsCats       :: Map Label (Visibility, MetaCategory)
    }

{-
    Rules of these collections; note that the meta-model is *not* a
    multiple inheritance model.

    ∃ MetaClass A, B : A.clsSuper = B ↔ A ∈ B.clsSupClasses

-}

data MetaMethod = MetaMethod
    { methodParams  :: Params
    , methodInvoke  :: [Val] -> Eval Val
    }

data MetaProperty = MetaProperty
    { propType          :: Type
    , propDefault       :: Eval Val
    }

{-
  The old association metametaclass...

data MetaAssoc = MetaAssoc
    { assocSource       :: MetaClass
    , assocTarget       :: MetaClass
    , assocSourceRange  :: Range
    , assocTargetRange  :: Range
    , assocCategory     :: Category
    , assocIsComposite  :: Bool     -- if you kill this, its children
                                    -- makes no sense to live either
    }
-}

{-
    This is a bit like an association, but easier to deal with for
    writing proofs.
-}
data MetaCategory = MetaCategory
    { catClass       :: MetaClass
    , catPair        :: MetaCategory
    , catRange       :: Range
    , catIsComposite :: Bool        -- if you kill this, its children
                                    -- makes no sense to live either
    , catOrdered     :: Bool        -- default false
    , catKeyed       :: Bool        -- default false
    , catCompanion   :: Label
    }

{-

    ∃ MetaClass A, MetaCategory C : A.clsCats ∋ C ↔ C.catClass = A

    ∃ MetaCategory C₁, C₂ : C₁.catPair = C₂ ↔ C₂.catPair = C₁

    -- can't be composite both ways

    ∃ MetaCategory C₁, C₂ : C₁.catPair = C₂ ∧ C₁.catIsComposite
         → ¬(C₂.catIsComposite)

    -- this seems the simplest way to specify complementary categories

    ∃ MetaCategory C₁, C₂, MetaClass M₁, M₂
       : C₁.catPair = C₂ ∧ C₁.catClass = M₁ ∧ C₂.catClass = M₂
       → (   ∃ M₁.clsCats{C₂.catCompanion}
           ∧ ∃ M₂.clsCats{C₁.catCompanion}
           ∧ M₁.clsCats{C₂.catCompanion}[1] = C₁
           ∧ M₂.clsCats{C₁.catCompanion}[1] = C₂
           ∧ M₁.clsCats{C₂.catCompanion}[0] = M₂.clsCats{C₁.catCompanion}[0]
           )
-}
    
data Visibility = Public | Private

type Label = String

type Range = (Multi, Multi)

data Multi = Zero | One | Many

{-
  simple range sanity stuff... enforce ordering
    ∃ Range R : R[0] = One → R[1] ∈ ( One | Many )
    ∃ Range R : R[1] = One → R[0] ∈ ( Zero | One )
    ∃ Range R : R[0] = Many → R[1] = Many
    ∃ Range R : R[1] = Zero → R[1] = Zero

 -}

data Category = Unordered | Ordered | Keyed

data Type = Int | Str

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
        someone who has will come along later and fix this.  Or maybe
        I'll get through the book soon :).  Don't hold your breath...

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
	, pkgStash = MetaProperty { type = Map (sigil, Symbol) Object }
	}

  -- Package->has_many("pkgChildren" => Package)
  -- Package->maybe_has_one("pkgParent" => Package)
  Package.clsCats =
	{ pkgChildren = 
              (Public, MetaCategory
                { catIsComposite = true,
                  catRange = (Zero, One),
                  catCompanion = "pkgParent",
                  catPair = MetaCategory {
                     catClass = Package,
                     catRange = (Zero, Many),
                  },
                })
	}

{-
  Traits - just what do we know about them?  They're mentioned in S02,
           S04, etc as applying to Packages, Blocks, etc.  There is a
           *lot* in S06 on block traits...

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
	{ modTraits = (Public, MetaCategory
                      { catIsComposite = true,
                        catRange = (Zero, Many),
                        catCompanion = "pkgParent",
                        catKeyed = true,
                        catPair = MetaCategory
                                   ( { catClass = PkgTrait,
                                       catRange = (One, One) } ),
                      })
	}
  
  Class := MetaClass where clsName = "Class"
  Class.clsAssocs =
        { isa = (Public, MetaCategory
                         { catOrdered = true,
                           catRange = (Zero, Many),
                           catCompanion = "subClasses",
                           catPair = MetaCategory
                                     { catRange = (Zero, Many),
                                       catClass = Class }
                         }),
          methods = (Public, MetaCategory
                             { catKeyed = true,
                               catRange = (Zero, Many),
                               catCompanion = "Class",
                               catPair = MetaCategory
                                         { catRange = (One, One),
                                           catClass = Method
                                         } }),
        }

  

  -- starting to look like the beginning again?  :)

  ∃ Class C₁, C₂ : C₁.superClasses ∋ C₂
                 ↔ C₂.subClasses ∋ C₁ ∧ C₂ ∉ C₁.subClasses

  -- hmm, anyone know how to induct the above to disallow circular inheritance?

  -- perhaps stating axioms here isn't the right place, anyway.
  -- & (reading TAPL)

-}

