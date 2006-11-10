{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Compile.Role where

import MO.Base
import MO.Compile
import MO.Run
import MO.Util
import MO.Compile.Attribute

-- Delay further abstractions until we need ;)
data Role m = MkRole
    { roRoles                  :: [Role m]
    , roAttributes             :: [Attribute m]
    , roPublicMethods          :: Collection (AnyMethod m)
    , roPrivateMethods         :: Collection (AnyMethod m)
    }
    deriving (Eq)

emptyRole = MkRole
          { roRoles                  = [] 
          , roAttributes             = []
          , roPublicMethods          = emptyCollection
          , roPrivateMethods         = emptyCollection
          } 

parent_roles :: Role m -> [Role m]
parent_roles = roRoles

role_public_methods     = roPublicMethods
role_private_methods    = roPrivateMethods
role_attributes         = roAttributes

all_using_role_shadowing :: (Show a, Ord a) => Role m -> (Role m -> Collection a) -> Collection a
all_using_role_shadowing r f = sym_shadowing r parent_roles f

all_using_role_inheritance :: (Show a, Ord a) => Role m -> (Role m -> Collection a) -> Collection a
all_using_role_inheritance r f = sym_inheritance r parent_roles f
