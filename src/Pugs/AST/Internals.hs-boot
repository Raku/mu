{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances #-}

module Pugs.AST.Internals where
import Pugs.Types
import Pugs.Internals
import Pugs.AST.Pos
import Pugs.AST.SIO
import Pugs.Cont hiding (shiftT, resetT)
import Control.Concurrent.STM
import Data.Dynamic

data Env
data Val
data VRef
data VObject
data PadEntry
-- newtype Pad = MkPad { padEntries :: Map Var PadEntry }
newtype ObjectId = MkObjectId { unObjectId :: Int }

type VType = Type
type VArray = [Val]
type VList = [Val]
type VHash = Map VStr Val

envPos' :: Env -> Pos
errStrPos :: VStr -> Pos -> Val
errValPos :: Val -> Pos -> Val
envAtomic :: Env -> Bool
envContext :: Env -> Cxt
envMaxId :: Env -> TVar ObjectId
envClasses :: Env -> ClassTree
enterAtomicEnv :: Env -> Env
objOpaque :: VObject -> Maybe Dynamic

createObjectRaw :: (MonadSTM m)
    => ObjectId -> Maybe Dynamic -> VType -> [(VStr, Val)] -> m VObject
