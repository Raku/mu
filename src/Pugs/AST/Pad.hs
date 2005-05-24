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

subPad :: VCode -> Pad
subPad sub = maybe (mkPad []) envLexical (subEnv sub)

-- | Return the difference between two pads.
diffPads :: Pad -- ^ Pad a
         -> Pad -- ^ Pad b
         -> Pad -- ^ a - b
diffPads (MkPad map1) (MkPad map2) = MkPad $ Map.difference map1 map2

unionPads :: Pad -> Pad -> Pad
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.union map1 map2

updateSubPad :: VCode -> (Pad -> Pad) -> VCode
updateSubPad sub f = sub
    { subEnv = fmap (\e -> e{ envLexical = f (subPad sub) }) (subEnv sub) 
    }
