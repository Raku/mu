{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Emit.PIR where
import Text.PrettyPrint
import Data.Char
import Data.Typeable
import Emit.Common

type PIR = [Decl]

data Decl
    = DeclSub   !SubName ![SubFlag] ![Stmt]
    | DeclNS    !PkgName
    | DeclInc   !FilePath
    deriving (Show, Eq, Typeable)

data Stmt
    = StmtComment   !String
    | StmtLine      !FilePath !Int
    | StmtIns       !Ins
    | StmtPad       ![(VarName, Expression)] ![Stmt]
    | StmtRaw       !Doc
    deriving (Show, Eq, Typeable)

data Ins
    = InsLocal      !RegType !VarName               -- ^ Inserts a @.local@ directive
    | InsNew        !LValue !ObjType                -- ^ Inserts a @new@ opcode
    | InsBind       !LValue !Expression             -- ^ Inserts a @set@ opcode
    | InsAssign     !LValue !Expression             -- ^ Inserts an @assign@ opcode
    | InsExp        !Expression
    | InsFun        ![Sig] !Expression ![Expression]
    | InsTailFun    !Expression ![Expression]
    | InsPrim       !(Maybe LValue) !PrimName ![Expression]
    | InsLabel      !LabelName                      -- ^ Inserts a label
    | InsComment    !String !(Maybe Ins)            -- ^ Inserts a comment
    deriving (Show, Eq, Typeable)

{-| Tags a PIR subroutine definition with @\@MAIN@, @\@LOAD@, @\@ANON@,
    @\@METHOD@, or @\@MULTI@. -}
data SubFlag = SubMAIN | SubLOAD | SubANON | SubMETHOD | SubMULTI [ObjType]
    deriving (Show, Eq, Typeable)

data RegType
    = RegInt                            -- ^ @I@ (integer) register
    | RegNum                            -- ^ @N@ (number) register
    | RegStr                            -- ^ @S@ (string) register
    | RegPMC                            -- ^ @P@ (PMC) register
    deriving (Show, Eq, Typeable)

data RelOp = RelLT | RelLE | RelEQ | RelNE | RelGE | RelGT
    deriving (Show, Eq, Typeable)

data ObjType    = PerlUndef | PerlArray | PerlHash
    deriving (Show, Eq, Typeable, Read)

type LabelName  = String
type SubName    = String
type VarName    = String
type PrimName   = String
type PkgName    = String
type CallConv   = String

data LValue
    = VAR !VarName        -- ^ A variable declared by @.local@
    | PMC !Int            -- ^ PMC register /n/
    | STR !Int            -- ^ String register /n/
    | INT !Int            -- ^ Integer register /n/
    | NUM !Int            -- ^ Number register /n/
    deriving (Show, Eq, Typeable)

data Expression
    = ExpLV !LValue
    | ExpLit !Literal
    | ExpKeyed !LValue !Expression
    deriving (Show, Eq, Typeable)

data Literal
    = LitStr !String      -- ^ A literal string
    | LitInt !Integer     -- ^ A literal integer
    | LitNum !Double      -- ^ A literal number
    deriving (Show, Eq, Typeable)

instance Emit Decl where
    emit (DeclNS name) = emit ".namespace" <+> brackets (quotes $ emit name)
    emit (DeclInc name) = emit ".include" <+> (quotes $ emit name)
    emit (DeclSub name styps stmts)
        =  (emit ".sub" <+> doubleQuotes (emit name) <+> commaSep styps)
        $+$ nested (emitBody stmts)
        $+$ emit ".end"
        where
        emitBody [] = []
        emitBody [(StmtIns (InsFun _ name args))] = [emit $ StmtIns (InsTailFun name args)]
        emitBody (x:xs) = emit x : emitBody xs

instance Emit SubFlag where
    emit = emit . ('@':) . drop 3 . show

curPad :: Int
curPad = -1

instance Emit Stmt where
    emit (StmtComment []) = empty
    emit (StmtComment str) = vcat [ emit "###" <+> emit line | line <- lines str ]
    emit (StmtLine file line) = text "#line" <+> doubleQuotes (emit file) <+> emit line
    emit (StmtIns ins) = emit ins
    emit (StmtPad pad _) = vcat $
        [ emit "new_pad" <+> int curPad
        ] ++ map (\(var, exp) -> emit ("store_lex" .- [lit (-1 :: Int), lit var, exp])) pad
    emit (StmtRaw doc) = doc

instance Emit RegType where
    emit = emit . map toLower . drop 3 . show

instance Emit Ins where
    emit (InsLocal rtyp name) = emit ".local" <+> emit rtyp <+> emit name
    emit (InsNew ident otyp) = eqSep ident "new" [otyp]
    emit (InsAssign ident lit) = eqSep ident "assign" [lit]
    emit (InsBind ident lit) = eqSep ident "set" [lit]
    emit (InsPrim (Just ret) name args) = eqSep ret name args
    emit (InsPrim Nothing "store_lex" (_:args)) =
        -- XXX - horrible hack! perl 4!
        emit (InsPrim Nothing "store_global" args)
    emit (InsPrim Nothing name args) = emit name <+> commaSep args
    emit (InsFun rets (ExpLit (LitStr name)) args) = emitFunName "invokecc" name args rets
    emit (InsFun rets fun args) = emitFun "invokecc" fun args rets
    emit (InsTailFun (ExpLit (LitStr name)) args) = emitFunName "tailcall" name args []
    emit (InsExp _) = empty
    emit (InsLabel label) = nest (-2) (emit label <> colon)
    emit (InsComment comment ins) = emit (StmtComment comment) $+$ emit ins
    emit x = error $ "can't emit ins: " ++ show x

emitRets :: [Sig] -> Doc
emitRets [] = empty
emitRets rets = emit ("get_results" .- sigList rets)

emitFun :: (Emit b, Emit c) => CallConv -> b -> [c] -> [Sig] -> Doc
emitFun callconv fun args rets = emitArgs args
    $+$ emitRets rets
    $+$ emit callconv <+> emit fun

emitArgs :: (Emit a) => [a] -> Doc
emitArgs args = emit "set_args" <+> commaSep (sig:map emit args)
    where
    sig = quotes $ parens (commaSep (replicate (length args) maybeFlatten))

emitFunName :: Emit b => CallConv -> String -> [b] -> [Sig] -> Doc
emitFunName callconv name args rets = eqSep (funPMC :: LValue) "find_name" [LitStr name]
    $+$ emitFun callconv (funPMC :: LValue) args rets

noArgs :: [Expression]
noArgs = []

instance Emit ObjType where
    emit = emit . ('.':) . show

instance Emit Expression where
    emit (ExpLV lhs) = emit lhs
    emit (ExpLit lit) = emit lit
    emit (ExpKeyed pmc idx) = emit pmc <> brackets (emit idx)

instance Emit LValue where
    emit (VAR name) = emit name
    emit (PMC num) = emit "$P" <> emit num
    emit (STR str) = emit "$S" <> emit str
    emit (INT str) = emit "$I" <> emit str
    emit (NUM str) = emit "$N" <> emit str

instance Emit Literal where
    emit (LitStr str) = text . show $ concatMap quoted str
        where
        quoted :: Char -> String
        quoted '\'' = "\\'"
        quoted '\\' = "\\\\"
        quoted x = [x]
    emit (LitInt int) = integer int
    emit (LitNum num) = double num

#ifndef HADDOCK
infixl 4 <--
infixl 9 -->
infixl 4 .-
infixl 4 <-&
infixl 4 .&
#endif

namespace :: PkgName -> Decl
include :: PkgName -> Decl
{-| Short for 'InsBind' (binding). -}
(<:=) :: LValue -> Expression -> Ins
{-| Short for 'InsAssign'. -}
(<==) :: LValue -> Expression -> Ins
{-| Calls an opcode which returns a value. -}
(<--) :: LValue -> PrimName -> [Expression] -> Ins
{-| Calls an opcode, ignoring any return values. -}
(.-) :: PrimName -> [Expression] -> Ins
{-| Calls an user-defined sub which returns a list of values. -}
(<-&) :: [Sig] -> Expression -> [Expression] -> Ins
{-| Calls an user-defined sub, ignoring any return values. -}
(.&) :: String -> [Expression] -> Ins

namespace = DeclNS
include = DeclInc

(<:=) = InsBind
(<==) = InsAssign
(<--) = InsPrim . Just
(.-)  = InsPrim Nothing
(<-&) = InsFun
(.&)  = InsFun [] . lit

{-| @$P0@ register -}
nullPMC :: (RegClass a) => a
nullPMC = reg $ PMC 0

{-| @$P1@ register -}
funPMC :: (RegClass a) => a
funPMC = reg $ PMC 1

{-| @$P2@ register -}
rv :: (RegClass a) => a
rv = reg $ PMC 2

{-| @$P10@ register -}
arg0 :: (RegClass a) => a
arg0 = reg $ PMC 10

{-| @$P11@ register -}
arg1 :: (RegClass a) => a
arg1 = reg $ PMC 11

{-| @$P12@ register -}
arg2 :: (RegClass a) => a
arg2 = reg $ PMC 12

{-| @$P13@ register -}
arg3 :: (RegClass a) => a
arg3 = reg $ PMC 13

{-| @$P8@ register -}
tempPMC :: (RegClass a) => a
tempPMC = reg $ PMC 8

{-| @$P9@ register -}
tempPMC2 :: (RegClass a) => a
tempPMC2 = reg $ PMC 9

{-| @$S8@ register -}
tempSTR :: (RegClass a) => a
tempSTR = reg $ STR 8

{-| @$S9@ register -}
tempSTR2 :: (RegClass a) => a
tempSTR2 = reg $ STR 9

{-| @$S10@ register -}
tempSTR3 :: (RegClass a) => a
tempSTR3 = reg $ STR 10

{-| @$I8@ register -}
tempINT :: (RegClass a) => a
tempINT = reg $ INT 8

{-| @$I9@ register -}
tempINT2 :: (RegClass a) => a
tempINT2 = reg $ INT 9

{-| @$I10@ register -}
tempINT3 :: (RegClass a) => a
tempINT3 = reg $ INT 10

{-| @$I11@ register -}
tempINT4 :: (RegClass a) => a
tempINT4 = reg $ INT 11

{-| @$N8@ register -}
tempNUM :: (RegClass a) => a
tempNUM = reg $ NUM 8

{-| @$N9@ register -}
tempNUM2 :: (RegClass a) => a
tempNUM2 = reg $ NUM 9

class RegClass y where
    reg :: LValue -> y

instance RegClass LValue where
    reg = id

instance RegClass Expression where
    reg = ExpLV

instance RegClass Sig where
    reg = MkSig [] . ExpLV

class LiteralClass x y | x -> y where
    lit :: x -> y 

instance LiteralClass Doc Expression where
    lit = ExpLit . LitStr . render

instance LiteralClass String Expression where
    lit = ExpLit . LitStr

instance LiteralClass Int Expression where
    lit = ExpLit . LitInt . toInteger

instance LiteralClass Bool Expression where
    lit False = ExpLit $ LitInt 0
    lit True = ExpLit $ LitInt 1

instance LiteralClass Double Expression where
    lit = ExpLit . LitNum

{-| Subroutine declaration. -}
sub :: SubName         -- ^ Name of the subroutine
    -> [Sig]           -- ^ Signature
    -> [Ins]           -- ^ Subroutine body
    -> Decl            -- ^ The final subroutine declaration
sub name [] body = DeclSub name [] (map StmtIns body)
sub name sigs body = DeclSub name [] stmts
    where
    param = "get_params" .- sigList sigs
    stmts = map StmtIns (param:body)

sigList :: [Sig] -> [Expression]
sigList sigs = (flags:map sigIdent sigs)
    where
    flags = lit . render . parens . commaSep $ map sigFlags sigs

instance Emit [ArgFlag] where
    emit = emit . sum . map argVal

data Sig = MkSig
    { sigFlags  :: [ArgFlag]
    , sigIdent  :: Expression
    }
    deriving (Show, Eq, Typeable)

data ArgFlag
    = MkArgFlatten | MkArgSlurpyArray
    | MkArgMaybeFlatten | MkArgOptional
    deriving (Show, Eq, Typeable)

argVal :: ArgFlag -> Int
argVal MkArgFlatten      = 0x08
argVal MkArgSlurpyArray  = 0x08
argVal MkArgMaybeFlatten = 0x10
argVal MkArgOptional     = 0x20

maybeFlatten :: Doc
maybeFlatten = emit [MkArgMaybeFlatten]

{-| Marks a parameter as slurpy. -}
slurpy :: Expression -> Sig
slurpy = MkSig [MkArgSlurpyArray]

#ifndef HADDOCK
{-| Returns from a sub. -}
(-->) :: Decl -> [Expression] -> Decl
(DeclSub name styps stmts) --> rets = DeclSub name styps $ stmts ++ map StmtIns
    [ "set_returns" .- retSigList rets
    , "returncc" .- []
    ]
_ --> _ = error "Can't return from non-sub"
#endif

retSigList :: [Expression] -> [Expression]
retSigList rets = (lit sig : rets)
    where
    sig = parens (commaSep (replicate (length rets) maybeFlatten))

vop1 :: SubName -> PrimName -> Decl
vop1 p6name opname =
    sub p6name [arg0] 
      [ InsNew rv PerlUndef
      , rv <-- opname $ [arg0]
      ] --> [rv]
vop2 :: SubName -> PrimName -> Decl
vop2 p6name opname =
    sub p6name [arg0, arg1] 
      [ InsNew rv PerlUndef
      , rv <-- opname $ [arg0, arg1]
      ] --> [rv]

vop2keyed :: SubName -> LValue -> Decl
vop2keyed p6name temp =
    sub p6name [arg0, arg1] 
      [ temp    <:= arg1
      , rv      <:= ExpKeyed arg0 (ExpLV temp)
      ] --> [rv]

--vop1x :: (RegClass a, RegClass b) => SubName -> PrimName -> a -> b -> Decl
vop1x :: SubName -> PrimName -> (forall a. RegClass a => a) -> (forall b. RegClass b => b) -> Decl
vop1x p6name opname regr reg0 =
    sub p6name [arg0] 
      [ InsNew rv PerlUndef
      , reg0 <:= arg0
      , regr <-- opname $ [reg0]
      , rv <-- "assign" $ [regr]
      ] --> [rv]

vop2x :: SubName -> PrimName -> (forall a. RegClass a => a) -> (forall b. RegClass b => b) -> (forall c. RegClass c => c) -> Decl
vop2x p6name opname regr reg0 reg1 =
    sub p6name [arg0, arg1] 
      [ InsNew rv PerlUndef
      , reg0 <:= arg0
      , reg1 <:= arg1
      , regr <-- opname $ [reg0,reg1]
      , rv <-- "assign" $ [regr]
      ] --> [rv]

vop1ii :: SubName -> PrimName -> Decl
vop1ii p6name opname = vop1x p6name opname tempINT tempINT
vop1nn :: SubName -> PrimName -> Decl
vop1nn p6name opname = vop1x p6name opname tempNUM tempNUM

vop2iii :: SubName -> PrimName -> Decl
vop2iii p6name opname = vop2x p6name opname tempINT tempINT tempINT2 
vop2iss :: SubName -> PrimName -> Decl
vop2iss p6name opname = vop2x p6name opname tempINT tempSTR tempSTR2 
vop2nnn :: SubName -> PrimName -> Decl
vop2nnn p6name opname = vop2x p6name opname tempNUM tempNUM tempNUM2 

bare :: VarName -> Expression
bare = ExpLV . VAR

parrotBrokenXXX :: Bool
parrotBrokenXXX = True

-- calls and place result in tempPMC
callBlock :: VarName -> Expression -> [Ins]
callBlock label fun =
    [ "newsub" .- [funPMC, bare ".Continuation", bare label]
    ] ++ callBlockCC fun ++ collectCC label

collectCC :: LabelName -> [Ins]
collectCC label =
    [ InsLabel label
    , if parrotBrokenXXX
        then "find_global" .- [tempPMC, tempSTR]
        else "get_params" .- sigList [tempPMC]
    ]

callBlockCC :: Expression -> [Ins]
callBlockCC fun | parrotBrokenXXX =
    [ tempINT   <-- "get_addr" $ [fun]
    , tempSTR   <:= tempINT
    , "store_global" .- [tempSTR, funPMC]
    , "invoke" .- [fun]
    ]
callBlockCC fun =
    [ "set_args" .- sigList [funPMC]
    , "invoke" .- [fun]
    ]

stmtControlCond :: VarName -> PrimName -> Decl
stmtControlCond name comp = sub ("&statement_control:" ++ name) [arg0, arg1, arg2] (
    [ "newsub" .- [funPMC, bare ".Continuation", bare postL]
    , comp .- [arg0, bare altL]
    ] ++ callBlockCC arg1 ++
    [ InsLabel altL
    ] ++ callBlockCC arg2 ++ collectCC postL) --> [tempPMC]
    where
    altL = ("sc_" ++ name ++ "_alt")
    postL = ("sc_" ++ name ++ "_post")

op2Logical :: VarName -> PrimName -> Decl
op2Logical name comp = sub ("&infix:" ++ name) [arg0, arg1] (
    [ "newsub" .- [funPMC, bare ".Continuation", bare postL]
    , comp .- [arg0, bare altL]
    , "set_returns" .- retSigList [arg0]
    , "returncc" .- []
    , InsLabel altL
    ] ++ callBlockCC arg1 ++ collectCC postL) --> [tempPMC]
    where
    altL = ("sc_" ++ escaped name ++ "_alt")
    postL = ("sc_" ++ escaped name ++ "_post")

escaped :: String -> String
escaped = concatMap esc
    where
    esc :: Char -> String
    esc '|' = "_or_"
    esc '&' = "_and_"
    esc x = [x]

{-| The Prelude, defining primitives like @&say@, @&infix:+@, etc. -}
preludePIR :: Doc
preludePIR = emit $
    -- [ include "interpinfo.pasm"
    [ sub "&print" [slurpy arg0]
        [ tempSTR <-- "join" $ [lit "", arg0]
        , "print" .- [tempSTR]
        ] --> [lit True]
{- XXX BROKEN
    , sub "&say" [slurpy arg0]
        [ "push" .- [arg0, lit "\n"]
        , "&print" .& [arg0]
        ]
-}
    , sub "&say" [slurpy arg0]
        [ tempSTR <-- "join" $ [lit "", arg0]
        , "print" .- [tempSTR]
        , "print" .- [lit "\n"]
        ] --> [lit True]
    , sub "&exit" []
        [ "exit" .- [lit (0 :: Int)]
        ]
{-
    , sub "&run_END" []
        [ tempPMC <-- "find_global" $ [lit "@*END"]
        , InsLabel "run_END_loop"
        , tempINT <:= tempPMC
        , "le" .- [tempINT, ExpLit . LitInt $ 0, bare "run_END_done"]
        , tempPMC2 <-- "shift" $ [tempPMC]
        , InsFun [] tempPMC2 []
        , "goto" .- [bare "run_END_loop"]
        , InsLabel "run_END_done"
        ] --> [lit True]
-}
    -- Control flowy
    , sub "&statement_control:loop" [arg0, arg1, arg2, arg3] $
        [ InsLabel "sc_loop_next"
        ] ++ callBlock "loopCond" arg1 ++
        [ "unless" .- [tempPMC, bare "sc_loop_last"]
        ] ++ callBlock "loopBody" arg2 ++
        [ -- ..throw away the result of body...
        ] ++ callBlock "loopPost" arg3 ++
        [ "goto" .- [bare "sc_loop_next"]
        , InsLabel "sc_loop_last"
        , "returncc" .- []
        ]
    , stmtControlCond "if" "unless"
    , stmtControlCond "unless" "if"
    , op2Logical "&&" "if"
    , op2Logical "||" "unless"
    , op2Logical "and" "if"
    , op2Logical "or" "unless"
    , sub "&nothing" [] []

    -- Operators
    , sub "&infix:," [slurpy arg0]
        [] --> [arg0]
    , sub "&prefix:++" [arg0]
        [ "inc" .- [arg0]
        ] --> [arg0]
    , sub "&prefix:--" [arg0]
        [ "dec" .- [arg0]
        ] --> [arg0]
    , sub "&postfix:++" [arg0]
        [ InsNew rv PerlUndef
        , rv <-- "assign" $ [arg0]
        , "inc" .- [arg0]
        ] --> [rv]
    , sub "&postfix:--" [arg0]
        [ InsNew rv PerlUndef
        , rv <-- "assign" $ [arg0]
        , "dec" .- [arg0]
        ] --> [rv]
    , sub "&prefix:-" [arg0]
        [ InsNew rv PerlUndef
        , rv <-- "neg" $ [arg0]
        ] --> [rv]
    , vop2 "&infix:+" "add"
    , vop2 "&infix:-" "sub"
    , vop2 "&infix:*" "mul"
    , vop2 "&infix:/" "div"
    , vop2 "&infix:%" "mod"
    , vop2 "&infix:~" "concat"
    , vop1 "&prefix:!" "not"
    , vop1 "&not" "not"
    , vop2iii "&infix:<" "islt"
    , vop2iii "&infix:<=" "isle"
    , vop2iii "&infix:>" "isgt"
    , vop2iii "&infix:>=" "isge"
    , vop2iii "&infix:==" "iseq"
    , vop2iii "&infix:!=" "isne"
    , vop2iss "&infix:lt" "islt"
    , vop2iss "&infix:le" "isle"
    , vop2iss "&infix:gt" "isgt"
    , vop2iss "&infix:gt" "isge"
    , vop2iss "&infix:eq" "iseq"
    , vop2iss "&infix:ne" "isne"
    , vop1 "&prefix:?^" "bnot"
    , vop2keyed "&postcircumfix:{}" tempSTR
    , vop2keyed "&postcircumfix:[]" tempINT
    , sub "&prefix:+" [arg0]
        [ tempNUM <:= arg0
        , InsNew rv PerlUndef
        , rv <:= tempNUM
        ] --> [rv]
    , sub "&prefix:~" [arg0]
        [ tempSTR <:= arg0
        , InsNew rv PerlUndef
        , rv <:= tempSTR
        ] --> [rv]
    , sub "&true" [arg0]
        [ InsNew rv PerlUndef
        , rv <:= (ExpLit . LitInt) 1
        , "if" .- [arg0, bare "true_pmc_is_true"]
        , rv <:= (ExpLit . LitInt) 0
        , InsLabel "true_pmc_is_true"
        ] --> [rv]

    -- Strings
    , vop1x "&chars" "length"     tempINT tempSTR
    , vop1x "&bytes" "bytelength" tempINT tempSTR
    , sub "&substr" [arg0, arg1, arg2]
        [ tempSTR   <-- "set" $ [arg0]
        , tempINT   <-- "set" $ [arg1]
        , tempINT2  <-- "set" $ [arg2]
        , tempSTR2  <-- "substr" $ [tempSTR, tempINT, tempINT2]
        , InsNew rv PerlUndef
        , rv        <-- "set" $ [tempSTR2]
        ] --> [rv]
    , vop1x "&chr" "chr" tempSTR tempINT
    , vop1x "&ord" "ord" tempINT tempSTR

    -- Objects
    , sub "&undef" []
        [ InsNew rv PerlUndef
        ] --> [rv]
    , sub "&undefine" [arg0]
        [ InsNew tempPMC PerlUndef
        , arg0 <-- "assign" $ [tempPMC]
        ] --> [arg0]
    , vop1x "&defined" "defined" tempINT tempPMC
{- XXX saying  hash
-- causes error:imcc:syntax error, unexpected IREG, expecting '('
    , sub "&id" [arg0]
        [ InsNew rv PerlUndef
        , tempINT <-- "hash" $ [arg0]
        , rv <-- "assign" $ [tempINT]
        ] --> [rv]
-}
    , vop1 "&clone" "clone"

    -- Aggregates
    , sub "&pop" [arg0]
        [ rv <-- "pop" $ [arg0]
        ] --> [rv]
    , sub "&push" [arg0, arg1]
        [ "push" .- [arg0, arg1]
        ] --> [lit True]
    , sub "&delete" [arg0, arg1]
        [ rv      <:= ExpKeyed arg0 arg1
        , "delete" .- [ExpKeyed arg0 arg1]
        ] --> [rv]
    , sub "&exists" [arg0, arg1]
        [ tempINT <-- "exists" $ [ExpKeyed arg0 arg1]
        , InsNew rv PerlUndef
        , rv      <:= tempINT
        ] --> [rv]
    , sub "&join" [arg0, arg1]
        [ InsNew rv PerlUndef
        , tempSTR <:= arg0
        , tempSTR2 <-- "join" $ [tempSTR, arg1]
        , rv <-- "assign" $ [tempSTR2]
        ] --> [rv]

    --, namespace "Perl6::Internals"
    , sub "&Pugs::Internals::symbolic_deref" [arg0, slurpy arg1]
        -- find_name($arg0 ~ join "::", @arg1)
        [ tempSTR  <-- "join" $ [lit "::", arg1]
        , tempSTR2 <:= arg0
        , tempSTR  <-- "concat" $ [tempSTR2, tempSTR]
        -- XXX: Normalise tempSTR, i.e. "&infix:<+>" -> "&infix:+"
        , rv       <-- "find_name" $ [tempSTR]
        ] --> [rv]

    -- Supporting Math::Basic
    , sub "&abs" [arg0]
        [ InsNew rv PerlUndef
        , rv <-- "assign" $ [arg0]
        , "abs" .- [arg0]
        ] --> [rv]
    , vop1nn "&exp" "exp"
    , vop1nn "&ln" "ln"
    , vop1nn "&log2" "log2"
    , vop1nn "&log10" "log10"
    -- also need: rand()? sign()? srand() ? S29
    , vop1nn "&sqrt" "sqrt"
    -- Supporting Math::Trig
    , vop1 "&sin" "sin"
    , vop1 "&cos" "cos" -- works as vop1.  but not sin().  sigh.
    , vop1 "&tan" "tan"
    , vop1 "&sec" "sec"
    , vop1 "&asin" "asin"
    , vop1 "&acos" "acos"
    , vop1 "&atan" "atan"
    , vop1 "&asec" "asec"
    , vop1 "&sinh" "sinh"
    , vop1 "&cosh" "cosh"
    , vop1 "&tanh" "tanh"
    , vop1 "&sech" "sech"
    -- also need: cosec, cotan, acosec, acotan, asinh, acosh, atanh, cosech,
    --  cotanh, asech, acosech, acotanh. S29

    -- Supporting unspeced:
    , vop1nn "&ceil" "ceil"
    , vop1nn "&floor" "floor"
    , vop1ii "&fact" "fact"
    , vop2iii "&gcd" "gcd"
    , vop2iii "&lcm" "lcm"
    , vop2nnn "&pow" "pow"
    -- parrot has no times()
    , sub "&time" []
        [ InsNew rv PerlUndef
        , tempNUM  <-- "time" $ []
        , rv       <:= tempNUM
        -- Parrot's time returns seconds since 1970, but Perl 6's time
        -- returns seconds since 2000, so we've to compensate.
        , "sub" .- [rv, ExpLit . LitNum $ 946684800]
        ] --> [rv]

    --, namespace "Str"
    , sub "&split" [arg0, arg1]
        [ InsNew rv PerlUndef
        , tempSTR <:= arg0
        , tempSTR2 <:= arg1
        -- special case split("\n",...) to get Test.pm working
        , "ne" .- [tempSTR, lit "\n", bare "split_normally"]
        , InsNew rv PerlArray
        , tempINT <:= (ExpLit . LitInt $ 0)
        , tempINT4 <-- "length" $ [tempSTR]
        , InsLabel "split_loop"
        , tempINT2 <-- "index" $ [tempSTR2, tempSTR, tempINT]
        , "lt" .- [tempINT2, ExpLit . LitInt $ 0, bare "split_last"]
        , tempINT3 <-- "sub" $ [tempINT2, tempINT]
        , tempSTR3 <-- "substr" $ [tempSTR2, tempINT, tempINT3]
        , tempINT <-- "add" $ [tempINT2, tempINT4]
        , "push" .- [rv, tempSTR3]
        , "goto" .- [bare "split_loop"]
        , InsLabel "split_last"
        , tempSTR3 <-- "substr" $ [tempSTR2, tempINT]
        , "push" .- [rv, tempSTR3]
        , "goto" .- [bare "split_done"]
        , InsLabel "split_normally"
        -- end of special case
        , tempPMC <-- "split" $ [tempSTR, tempSTR2]
        , rv <-- "assign" $ [tempPMC]
        , InsLabel "split_done"
        ] --> [rv]

    --, namespace "bool" -- Namespaces have bugs in both pugs and parrot.
    , sub "&bool::true" []
        [] --> [lit True]
    , sub "&bool::false" []
        [] --> [lit False]
    ]

