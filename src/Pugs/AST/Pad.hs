module Pugs.AST.Pad (
  mkPad, subPad, diffPads, unionPads, updateSubPad,
) where
import Pugs.Internals
import Pugs.AST.Internals
import Pugs.Types
import qualified Data.Map as Map

{-|
Produce a 'Pad' from a list of bindings. The inverse of 'padToList'.

Not to be confused with the actual 'Pad' constructor @MkPad@.
-}
mkPad :: [(Var, [(TVar Bool, TVar VRef)])] -> Pad
mkPad = MkPad . Map.fromList

{-|
Retrieve a sub's lexical 'Pad' from its environment ('Env').

If the sub has no associated environment, an empty 'Pad' is returned.
-}
subPad :: VCode -> Pad
subPad sub = maybe (mkPad []) envLexical (subEnv sub)

{-|
Return the key-wise difference between two 'Pad's.

Any keys found in both pads are removed from the resulting pad, even if the two 
pads' values are different. Keys found only in the second pad are ignored.
-}
diffPads :: Pad -> Pad -> Pad
diffPads (MkPad map1) (MkPad map2) = MkPad $ Map.difference map1 map2

{-|
Return the key-wise union of two 'Pad's.

If the same key is found in both pads, the value in the first pad is preserved.
-}
unionPads :: Pad -> Pad -> Pad
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.union map1 map2

{-|
Apply a 'Pad'-transformer to the given sub's lexical pad, producing a 'VCode'
with the new pad.
-}
updateSubPad :: VCode        -- ^ Initial sub
             -> PadMutator   -- ^ 'Pad'-transforming function
             -> VCode        -- ^ Sub with altered lexical pad
updateSubPad sub f = sub
    { subEnv = fmap (\e -> e{ envLexical = f (subPad sub) }) (subEnv sub) 
    }
