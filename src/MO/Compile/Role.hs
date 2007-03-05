{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Compile.Role where

import MO.Base ()
import MO.Compile
import MO.Run ()
import MO.Util
import MO.Compile.Attribute

-- Delay further abstractions until we need ;)
data Role m = MkRole
    { r_roles                  :: [Role m]
    , r_attributes             :: [Attribute m]
    , r_public_methods         :: Collection (AnyMethod m)
    , r_private_methods        :: Collection (AnyMethod m)
    }
    deriving (Eq)

emptyRole :: Role m
emptyRole = MkRole
          { r_roles                  = [] 
          , r_attributes             = []
          , r_public_methods         = emptyCollection
          , r_private_methods        = emptyCollection
          } 

parent_roles :: Role m -> [Role m]
parent_roles = r_roles

role_public_methods, role_private_methods :: Role m -> Collection (AnyMethod m)
role_public_methods     = r_public_methods
role_private_methods    = r_private_methods

role_attributes :: Role m -> [Attribute m]
role_attributes         = r_attributes

all_using_role_shadowing :: (Show a, Ord a) => Role m -> (Role m -> Collection a) -> Collection a
all_using_role_shadowing r f = sym_shadowing r parent_roles f

all_using_role_inheritance :: (Show a, Ord a) => Role m -> (Role m -> Collection a) -> Collection a
all_using_role_inheritance r f = sym_inheritance r parent_roles f
