{-# OPTIONS -fglasgow-exts #-}

-- Control operators for delimited continuations 
-- (implemented using 2-level CPS)
-- Transformer

module CC_2CPST (
  CC, Prompt, SubCont, 
  runCC,
  newPrompt, pushPrompt, -- operations on prompts
  letSubCont, pushSubCont, -- operations on subcontinuations
) where

import qualified PromptTR
import SeqTR
import Control.Monad.Trans

import Debug.Trace

----------------------------------------------------------------------
-- Types

newtype Cont m r a b = Cont (a -> MC r m b)
type MetaCont r m a b = Seq (Cont m) r a b

newtype CC r m a = CC (forall b. Cont m r a b -> MC r m b)
newtype MC r m b = MC (forall ans. MetaCont r m b ans -> PromptTR.P r m ans)
type Prompt r a = PromptTR.Prompt r a 
type SubCont r m a b = Seq (Cont m) r a b

----------------------------------------------------------------------

instance Monad m => Monad (CC r m) where  
  return e = CC (\ (Cont k) -> k e)
  (CC e1) >>= e2 = CC (\k -> e1 (Cont (\v1 -> let CC c = e2 v1 in c k)))

instance MonadTrans (CC r) where
    lift m = CC (\ (Cont k) -> 
		 MC (\mk -> lift (m >>= (\a -> let MC mc = k a
					       in PromptTR.runP (mc mk)))))

instance (MonadIO m) => MonadIO (CC r m) where
    liftIO = lift . liftIO

-- The previous equation was derived to guarantee the law
-- that runCC (lift m) === m
-- Indeed, runCC (lift m) reduces to the following
--runCC ce = PromptTR.runP (lift (m >>= (\a -> PromptTR.runP (return a))))


runC :: Monad m => (Cont m r a a -> MC r m a) -> MC r m a
runC e = e (Cont (\v -> MC (\mk -> appmk mk v)))

runCC :: Monad m => (forall r. CC r m a) -> m a
runCC ce = PromptTR.runP (let CC e = ce 
                              MC me = runC e
                          in me (EmptyS id))

appmk :: Monad m => MetaCont r m a ans -> a -> PromptTR.P r m ans
--appmk x y | trace "in appmk" False = undefined
appmk (EmptyS f) e = return (f e)
appmk (PushP _ sk) e = appmk sk e
appmk (PushSeg (Cont k) sk) e = let MC mc = k e in mc sk
appmk (PushCO f sk) e = appmk sk (f e)

----------------------------------------------------------------------
-- Exported operations 

newPrompt :: Monad m => CC r m (Prompt r a)
newPrompt = CC (\ (Cont k) -> MC (\mk -> do p <- PromptTR.newPrompt
                                            let MC me = k p 
                                            me mk))

pushPrompt :: Monad m => Prompt r a -> CC r m a -> CC r m a
pushPrompt p (CC e) = 
    CC (\k -> MC (\mk -> let MC me = runC e
                         in me (PushP p (PushSeg k mk))))
    
letSubCont :: Monad m => 
	      Prompt r b -> (SubCont r m a b -> CC r m b) -> CC r m a
letSubCont p f = 
    CC (\k -> MC (\mk -> 
       let (subk,mk') = splitSeq p mk
           CC e = f (PushSeg k subk)
           MC me = runC e
       in me mk'))

pushSubCont :: Monad m => SubCont r m a b -> CC r m a -> CC r m b
pushSubCont subk (CC e) = 
    CC (\k -> MC (\mk -> 
       let MC me = runC e
       in me (appendSeq subk (PushSeg k mk))))

----------------------------------------------------------------------

