{-# OPTIONS_GHC -fglasgow-exts -fparr #-}
module Pugs.AST.Pad (
  mkPad, unionPads, padKeys, filterPad, adjustPad, mergePadEntry,
  mergeLexPads, readMPad, writeMPad, appendMPad, modifyMPad, newMPad
) where
import Pugs.Internals
import Pugs.AST.SIO
import Pugs.AST.Internals
import Pugs.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

instance Monoid Pad where
    mempty  = emptyPad
    mappend = unionPads
    mconcat = MkPad . Map.unionsWith mergePadEntry . map padEntries

{-|
Produce a 'Pad' from a list of bindings. The inverse of 'padToList'.

Not to be confused with the actual 'Pad' constructor @MkPad@.
-}
mkPad :: [(Var, PadEntry)] -> Pad
mkPad = listToPad

{-|
Merge multiple (possibly mutable) pads into one.
-}
mergeLexPads :: MonadSTM m => LexPads -> m Pad
mergeLexPads chain = stm $ do
    pads <- forM chain $ \lpad -> case lpad of
        PRuntime p      -> return p
        PCompiling p    -> readMPad p
    return . MkPad $ Map.unionsWith mergePadEntry (map padEntries pads)

readMPad :: MonadSTM m => MPad -> m Pad
readMPad = stm . readTVar . mp_pad

writeMPad :: MonadSTM m => MPad -> Pad -> m ()
writeMPad mp p = stm $ writeTVar (mp_pad mp) p

appendMPad :: MonadSTM m => MPad -> Pad -> m ()
appendMPad mp p = stm $ modifyTVar (mp_pad mp) (`unionPads` p)

modifyMPad :: MonadSTM m => MPad -> (Pad -> Pad) -> m ()
modifyMPad mp f = stm $ modifyTVar (mp_pad mp) f

newMPad :: MonadSTM m => Pad -> m MPad
newMPad p = do
    tvar <- stm $ newTVar p
    return $ MkMPad (addressOf tvar) tvar

{-
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
-}

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
    | Just (newMC :: VMultiCode) <- fromTypeable newCV
    , Just (oldMC :: VMultiCode) <- fromTypeable oldCV
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
