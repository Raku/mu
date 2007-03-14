{-# OPTIONS_GHC -fglasgow-exts -fparr #-}
module Pugs.AST.Pad (
  mkPad, subPad, diffPads, unionPads, padKeys, filterPad, adjustPad, mergePadEntry
) where
import Pugs.Internals
import Pugs.AST.Internals
import Pugs.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

{-|
Produce a 'Pad' from a list of bindings. The inverse of 'padToList'.

Not to be confused with the actual 'Pad' constructor @MkPad@.
-}
mkPad :: [(Var, PadEntry)] -> Pad
mkPad = listToPad

{-|
Retrieve a sub's lexical 'Pad' from its environment ('Env').

If the sub has no associated environment, an empty 'Pad' is returned.
-}
subPad :: VCode -> Pad
subPad sub = maybe (mkPad []) envLexical (subEnv sub)

{-|
Return the difference between two 'Pad's.

Any keys found in both pads that has identical values are removed from the
resulting pad.  Keys found only in the second pad are ignored.
-}
diffPads :: Pad -> Pad -> Pad
diffPads (MkPad map1) (MkPad map2) = MkPad $ Map.differenceWith diffPadEntry map1 map2
    where
    diffPadEntry x y | x == y    = Nothing
                     | otherwise = Just x

{-|
Return the key-wise union of two 'Pad's.

If the same key is found in both pads, merging multi subs into one.
-}
unionPads :: Pad -> Pad -> Pad
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.unionWith mergePadEntry map1 map2

adjustPad :: (PadEntry -> PadEntry) -> Var -> Pad -> Pad
adjustPad f v (MkPad p) = MkPad (Map.adjust f v p)

mergePadEntry :: PadEntry -> PadEntry -> PadEntry
mergePadEntry
    PEConstant{ pe_proto = MkRef (ICode newCV), pe_flags = flags }
    PEConstant{ pe_proto = MkRef (ICode oldCV) }
    | Just newMC <- fromTypeable newCV
    , Just oldMC <- fromTypeable oldCV
    = PEConstant
        { pe_type  = mc_type newMC -- XXX - Select a narrower type?
        , pe_proto = MkRef . ICode $! MkMultiCode
            { mc_type       = mc_type newMC
            , mc_subtype    = mc_subtype newMC
            , mc_assoc      = code_assoc newMC `mappend` code_assoc oldMC
            , mc_signature  = if length (mc_signature newMC) == length (code_params oldMC)
                then code_params newMC
                else [defaultArrayParam]
            , mc_variants   = mc_variants newMC `Set.union` mc_variants oldMC
            }
        , pe_flags          = flags
        }
mergePadEntry x _ = x

padKeys :: Pad -> Set Var
padKeys (MkPad pad) = Map.keysSet pad

filterPad :: (Var -> Bool) -> Pad -> Pad
filterPad f (MkPad pad) = MkPad (Map.filterWithKey (\k _ -> f k) pad)
