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
    , roInstanceMethods        :: Collection (AnyMethod m)
    , roPrivateInstanceMethods :: Collection (AnyMethod m)
    , roClassMethods           :: Collection (AnyMethod m)
    , roPrivateClassMethods    :: Collection (AnyMethod m)
    }
    deriving (Eq)

emptyRole = MkRole
          { roRoles                  = [] 
          , roAttributes             = []
          , roInstanceMethods        = emptyCollection
          , roPrivateInstanceMethods = emptyCollection
          , roClassMethods           = emptyCollection
          , roPrivateClassMethods    = emptyCollection
          } 

parent_roles :: Role m -> [Role m]
parent_roles = roRoles

role_instance_methods = roInstanceMethods
role_class_methods = roClassMethods

all_using_role_shadowing :: Ord a => Role m -> (Role m -> Collection a) -> Collection a
all_using_role_shadowing r f = sym_shadowing r parent_roles f

all_using_role_inheritance :: Ord a => Role m -> (Role m -> Collection a) -> Collection a
all_using_role_inheritance r f = sym_inheritance r parent_roles f
