{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns #-}
module Mildew.AST2IR where
import           Compiler.Hoopl
import           Control.Monad
import qualified Data.Map       as M

import qualified Mildew.AST as A
import qualified Mildew.IR  as I

type CU = CheckingFuelMonad (SimpleUniqueMonad)
type IdLabelMap = M.Map String Label
data LabelMapM a = LabelMapM (IdLabelMap -> CU (IdLabelMap, a))
instance Monad LabelMapM where
  return x = LabelMapM (\m -> return (m, x))
  LabelMapM f1 >>= k = LabelMapM (\m -> do (m', x) <- f1 m
                                           let (LabelMapM f2) = k x
                                           f2 m')
labelFor l = LabelMapM f
  where f m = case M.lookup l m of
                Just l' -> return (m, l')
                Nothing -> do l' <- freshLabel
                              let m' = M.insert l l' m
                              return (m', l')

freshLabel' = LabelMapM f
    where f m = do
                    l' <- freshLabel
                    return (m,l')

getBody graph = LabelMapM f
  where f m = return (m, graph)

run (LabelMapM f) = f M.empty >>=  return . snd

labelFor :: String -> LabelMapM Label
getBody  :: forall n. Graph n C C   -> LabelMapM (Graph n C C)
run      :: LabelMapM a -> CU a


expr e = case e of 
    A.Reg r -> I.Reg r
    A.IntegerConstant i -> I.IntegerConstant i
    A.StringConstant s -> I.StringConstant s
    x -> error $ (show x) ++ " is not an expression"


open ((A.Assign (A.Reg target) (A.Call ident (A.Capture invocant pos named) )):rest) = do
    label <- freshLabel'
    restGraph <- open rest
    return $ 
        mkLast (I.Call (I.Reg target) (expr ident) (expr invocant) (map expr pos) (map expr named) (Just label) Nothing Nothing)
        |*><*| 
        mkLabel label
        <*> restGraph

open ((A.Assign (A.Reg r) e):rest) = do
    restGraph <- open rest
    return $ mkMiddle (I.Assign r (expr e)) <*> restGraph

open (A.Reg _:rest) = open rest
open [] = return $ emptyGraph
open x = error $ "unknown node {" ++ (show x) ++ "}"

{-open ((A.Label lbl):rest) = do
    label <- labelFor lbl
    restGraph <- open rest
    return $ (mkMiddle $ I.Goto label) |*><*| (mkLabel label <*> restGraph)
-}

closed ((A.Label lbl):rest) = do
    label <- labelFor lbl
    restGraph <- open rest
    return $ mkLabel label <*> restGraph

closed _ = error "Jumping to something other then a label is an error"
    
    




