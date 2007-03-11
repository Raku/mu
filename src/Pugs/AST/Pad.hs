{-# OPTIONS_GHC -fglasgow-exts -fparr #-}
module Pugs.AST.Pad (
  mkPad, subPad, diffPads, unionPads, updateSubPad, mergePadEntry, padKeys, filterPad
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
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.unionWithKey mergePadEntry map1 map2

mergePadEntry :: Var -> PadEntry -> PadEntry -> PadEntry
mergePadEntry MkVar{ v_sigil = SCodeMulti } x y = EntryConstant
    { pe_type  = pe_type x -- XXX - Select a narrower type?
    , pe_value = MkRef . ICode $! MkMultiCode
        { mc_type       = pe_type x
        , mc_assoc      = assocOf x `mappend` assocOf y 
        , mc_signature  = case (paramOf x, paramOf y) of
            (Nothing, Nothing)  -> [defaultArrayParam]
            (Just x,  Nothing)  -> x
            (Nothing, Just y)   -> y
            (Just x, Just y)    -> if length x == length y then x else [defaultArrayParam] -- XXX - properly unify!
        , mc_variants   = variantsOf x +:+ variantsOf y
        }
    }
    where
    paramOf entry = case pe_value entry of
        MkRef (ICode c) -> Just (code_params c)
        _               -> Nothing
    assocOf entry = case pe_value entry of
        MkRef (ICode c) -> code_assoc c
        _               -> mempty
    assocOf _ = mempty
    variantsOf e@EntryConstant{ pe_value = MkRef r } = case r of
        ICode c | Just cset <- fromTypeable c -> mc_variants cset
        _ -> [:e:]
    variantsOf e = [:e:]
mergePadEntry _ x _ = x

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

padKeys :: Pad -> Set Var
padKeys (MkPad pad) = Map.keysSet pad

filterPad :: (Var -> Bool) -> Pad -> Pad
filterPad f (MkPad pad) = MkPad (Map.filterWithKey (\k _ -> f k) pad)
