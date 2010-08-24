{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs, EmptyDataDecls, PatternGuards, TypeFamilies, NamedFieldPuns,StandaloneDeriving #-}
module Mildew.IR (Insn (..),Expr (..)) where

import Compiler.Hoopl


data Insn e x where
  Label  :: Label  ->                               Insn C O
  Assign :: String -> Expr    ->                    Insn O O
  Store  :: Expr   -> Expr    ->                    Insn O O
  Goto   :: Label  ->                               Insn O C
  Cond   :: Expr   -> Label   -> Label  ->          Insn O C
  Call   :: Expr  -> Expr  -> Expr -> [Expr] -> [Expr] -> Maybe Label -> Maybe Label -> Maybe Label -> Insn O C


deriving instance Show (Insn e x)

data Expr = Reg String | IntegerConstant Int | StringConstant String | Block [Expr] [String] deriving (Show)

instance NonLocal Insn where
  entryLabel (Label l) = l
  successors (Goto l) = [l]
  successors (Cond _ t f) = [t,f]
  successors (Call _ _  _ _ _ success control err) = concatMap list [success,control,err]
    where list (Just x) = [x]
          list (Nothing) = []

instance HooplNode Insn where
    mkBranchNode label = Goto label
    mkLabelNode label = Label label
    
