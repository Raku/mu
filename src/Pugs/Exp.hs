{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -cpp #-}
{-! global : YAML_Pos, Perl6Class, MooseClass !-}

{-| Capture-enabled "new" Pugs AST
 -}

module Pugs.Exp where

import Pugs.Internals
import {-# SOURCE #-} qualified Pugs.AST.Internals (Exp)
import Pugs.Val


type ExpVar = Var
type ExpVal = Val
type ExpCapt = Capt Exp

newtype ExpEmeritus = MkExpEmeritus { ee :: Pugs.AST.Internals.Exp }

instance Eq ExpEmeritus where _ == _ = True
instance Ord ExpEmeritus where compare _ _ = EQ
instance Show ExpEmeritus where show _ = "<Exp.Emeritus>"

-- | AST for an expression.
data Exp
    = ENoop                            -- ^ No-op
    | EVar      ExpVar                 -- ^ Variable
    | EVal      ExpVal                 -- ^ Value
    | EDeref    ExpVar                 -- ^ Dereference
    | EBind     Exp  Exp               -- ^ Bind, i.e., :=
    | EAssign   Exp  Exp               -- ^ Assignment, =
    | EControl  ExpControl             -- ^ Control structure, e.g. if, while
    | EFlatten  [Exp]                  -- ^ Wrapper for expressions forced into
                                       --   slurpy context
    | EE ExpEmeritus
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


-- | Control statement, such as "if".
data ExpControl
    = CCall        ID     ExpCapt       -- ^ lookup a routine, call it
    | CApply       Exp    ExpCapt       -- ^ apply a Code without lookup
    | CCond        Exp    Code          -- ^ 2 if 1
    | CTrenaryCond Exp    Code  Code    -- ^ 1 ?? 2 !! 3
    | CCondBlock   (Exp, Code) [(Exp, Code)] (Maybe Code)
                                        -- ^ if 1 { 2 } else { 3 } or in general,
                                        --   if 1 { 2 } elsif 3 { 4 } elsif 5 { 6 } 7
                                        -- ^ &statement_control:<if>
    | CGoto        ID                   -- ^ &statement_control:<goto>
    | CWhile       Exp  Code            -- ^ &statement_control:<while>
    | CGiven       Exp  Code            -- ^ given
    | CWhen        Exp  Code            -- ^ when
    | CForeign                          -- ^ &statement_control:<mycontrol>
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | AST for a statement. The top level of an AST is a list of Stmt.
data Stmt = MkStmt
    { label      :: Maybe ID
    , pragmas    :: Table
    , expression :: Exp
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | Carry over last pragmas and create a new statement out of an expression
nextStmt :: Stmt -> Exp -> Stmt
nextStmt MkStmt{ pragmas=prag } exp = MkStmt{ label=Nothing, pragmas=prag, expression=exp }

