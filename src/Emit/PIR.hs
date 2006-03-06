{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

{-|
    Parrot PIR syntax tree.

>   All that is gold does not glitter,
>   Not all those who wander are lost;
>   The old that is strong does not wither,
>   Deep roots are not reached by the frost.
-}

module Emit.PIR (
    module Emit.PIR,
    module Emit.Common,
) where
import Data.Char
import Data.List
import Data.Typeable
import Emit.Common
import DrIFT.YAML
import Text.PrettyPrint

{-! global : YAML !-}

{-| PIR code consists of declarations. -}
type PIR = [Decl]

data Decl
    = DeclSub
        { dsName    :: !SubName
        , dsFlags   :: ![SubFlag]
        , dsBody    :: ![Stmt]
        } -- ^ Subroutine declaration
    | DeclNS
        { dnPackage :: !PkgName
        , dnBody    :: ![Decl]
        } -- ^ Namespace declaration
    | DeclInc
        { diFile    :: !FilePath
        } -- ^ @.include@ directive
    deriving (Show, Eq, Typeable)

data Stmt
    = StmtComment   !String                  -- ^ Comment
    | StmtLine      !FilePath !Int           -- ^ @#line@ directive
    | StmtPad       ![(VarName, Expression)] ![Stmt]    -- ^ Lexical Pad
    | StmtRaw       !Doc                     -- ^ Backdoor into raw @Doc@
    | StmtIns       !Ins                     -- ^ Generic instructions
    | StmtSub       !SubName ![Stmt]         -- ^ Inner subroutine
    deriving (Show, Eq, Typeable)

data Ins
    = InsLocal      !RegType !VarName               -- ^ @.local@ directive
    | InsNew        !LValue !ObjType                -- ^ @new@ opcode
    | InsBind       !LValue !Expression             -- ^ @set@ opcode
    | InsAssign     !LValue !Expression             -- ^ @assign@ opcode
    | InsPrim       !(Maybe LValue) !PrimName ![Expression] -- ^ Other opcodes
    | InsFun        ![Sig] !Expression ![Expression]-- ^ Function call
    | InsTailFun    !Expression ![Expression]       -- ^ Tail call
    | InsLabel      !LabelName                      -- ^ Label
    | InsComment    !String !(Maybe Ins)            -- ^ Comment
    | InsExp        !Expression                     -- ^ Generic expressions
    | InsConst      !LValue !ObjType !Expression    -- ^ Constant
    deriving (Show, Eq, Typeable)

data Expression
    = ExpLV !LValue       -- ^ Variables
    | ExpLit !Literal     -- ^ Literals
    deriving (Show, Eq, Typeable)

data LValue
    = VAR !VarName        -- ^ A variable declared by @.local@
    | PMC !Int            -- ^ PMC register /n/
    | STR !Int            -- ^ String register /n/
    | INT !Int            -- ^ Integer register /n/
    | NUM !Int            -- ^ Number register /n/
    | KEYED !LValue !Expression
    deriving (Show, Eq, Typeable)

data Literal
    = LitStr !String      -- ^ A literal string
    | LitInt !Integer     -- ^ A literal integer
    | LitNum !Double      -- ^ A literal number
    deriving (Show, Eq, Typeable)

{-| Tags a PIR subroutine definition with @\@MAIN@, @\@LOAD@, @\@ANON@,
    @\@METHOD@, or @\@MULTI@. -}
data SubFlag = SubMAIN | SubLOAD | SubANON | SubMETHOD | SubMULTI ![ObjType] | SubOUTER !SubName
    deriving (Show, Eq, Typeable)

data RegType
    = RegInt                            -- ^ @I@ (Integer) register
    | RegNum                            -- ^ @N@ (Number) register
    | RegStr                            -- ^ @S@ (String) register
    | RegPMC                            -- ^ @P@ (PMC) register
    deriving (Show, Eq, Typeable)

{-| A PMC type, which, for example, can be given as an argument to the @new@
    opcode (e.g. @new .PerlScalar@). -}
data ObjType
    = PerlScalar | PerlArray | PerlHash
    | PerlInt | PerlPair | PerlRef | PerlEnv
    | Sub | Closure | Continuation
    | BareType String
    deriving (Show, Eq, Typeable)

type LabelName  = String
type SubName    = String
type VarName    = String
type PrimName   = String
type PkgName    = String
type CallConv   = String

{-| Emits PIR code for declarations (namespace, include, or sub declarations). -}
instance Emit Decl where
    emit (DeclNS name decls) = vcat
        [ emit ".namespace" <+> brackets (quotes $ emit name)
        , emit decls
        , emit ".namespace" <+> brackets (quotes $ emit "main")
        ]
    emit (DeclInc name) = emit ".include" <+> (quotes $ emit name)
    -- Perform λ-lifting here
    emit (DeclSub name styps stmts)
        =  (emit ".sub" <+> doubleQuotes (emit $ quoted name) <+> commaSep styps)
        $+$ nested (emitStmts stmts)
        $+$ emit ".end"
        $+$ emit [DeclSub name' [SubANON, SubOUTER name] body' | StmtSub name' body' <- stmts ]

emitStmts :: [Stmt] -> Doc
emitStmts stmts = vcat (emitLex:emitBody stmts)
    where
    emitBody [] = []
    emitBody [(StmtIns (InsFun _ name args))] = [emit $ StmtIns (InsTailFun name args)]
    emitBody (x:xs) = emit x : emitBody xs
    emitLex = vcat (map emitVar $ nub (concat [ pad | StmtPad pad _ <- stmts ]))
    emitVar :: (VarName, Expression) -> Doc
    emitVar (var, exp@(ExpLV (VAR name)))
        = emit (InsLocal RegPMC name)
        $+$ emit ".lex" <+> commaSep [emit (lit var), emit exp]
    emitVar _ = empty

{-| Emits PIR code for a 'SubFlag' (e.g. @:main@, @:anon@, etc.). -}
instance Emit SubFlag where
    emit (SubOUTER x) = colon <> text "outer" <> parens (doubleQuotes $ emit x)
    emit x = (emit . (':':) . map toLower . drop 3 . show) x

curPad :: Int
curPad = -1

instance Emit Stmt where
    emit (StmtComment []) = empty
    emit (StmtComment str) = vcat [ emit "###" <+> emit line | line <- lines str ]
    emit (StmtLine file line) = text "#line" <+> doubleQuotes (emit file) <+> emit line
    emit (StmtIns ins) = emit ins
    emit (StmtPad pad _) = vcat $
        map (\(var, exp) -> emit ("store_lex" .- [lit var, exp])) pad
        {-
        [ emit "new_pad" <+> int curPad
        ] ++ 
        -}
    emit (StmtRaw doc) = doc
    emit StmtSub{} = empty

instance Emit RegType where
    emit = emit . map toLower . drop 3 . show

instance Emit Ins where
    emit (InsLocal rtyp name) = emit ".local" <+> emit rtyp <+> emit name
    emit (InsNew ident otyp) = eqSep ident "new" [otyp]
    emit (InsAssign ident@(KEYED _ _) lit) = eqSep ident "" [lit] -- XXX questionable
    emit (InsAssign ident lit) = eqSep ident "assign" [lit]
    emit (InsBind ident@(KEYED _ _) lit) = eqSep ident "" [lit] -- XXX questionable
    emit (InsBind ident lit) = eqSep ident "set" [lit]
    emit (InsPrim (Just ret) name args) = eqSep ret name args
--  emit (InsPrim Nothing "store_lex" (_:args)) =
--      -- XXX - horrible hack! perl 4!
--      emit (InsPrim Nothing "store_global" args)
    emit (InsPrim Nothing name args) = emit name <+> commaSep args
    emit (InsFun rets (ExpLit (LitStr name)) args) = emitFunName "invokecc" name args rets
    emit (InsFun rets fun args) = emitFun "invokecc" fun args rets
    emit (InsTailFun (ExpLit (LitStr name)) args) = emitFunName "tailcall" name args []
    emit (InsTailFun fun args) = emitFun "tailcall" fun args []
    emit (InsExp _) = empty
    emit (InsConst ident rtyp lit) =
        emit ".const" <+> emit rtyp <+> emit ident <+> equals <+> emit lit
    emit (InsLabel label) = nest (-2) (emit label <> colon)
    emit (InsComment comment ins) = emit (StmtComment comment) $+$ emit ins

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

{-| Emits PIR code for an 'ObjType' (e.g. @.PerlScalar@). -}
instance Emit ObjType where
    emit PerlScalar = emit ".PerlUndef"  -- XXX special case
    emit PerlPair   = emit ".Pair"  -- XXX special case
    emit PerlRef    = emit ".Ref"   -- XXX special case
    emit (BareType x) = text $ ('.':x)
    emit x = emit . ('.':) . show $ x

instance Emit Expression where
    emit (ExpLV lhs) = emit lhs
    emit (ExpLit lit) = emit lit

instance Emit LValue where
    emit (VAR name) = emit name
    emit (PMC num) = emit "$P" <> emit num
    emit (STR str) = emit "$S" <> emit str
    emit (INT str) = emit "$I" <> emit str
    emit (NUM str) = emit "$N" <> emit str
    emit (KEYED pmc idx) = emit pmc <> brackets (emit idx)

{-| Emits a literal (a 'LitStr', 'LitInt', or 'LitNum'), and escapes if
    necessary. -}
instance Emit Literal where
    emit (LitStr str) = text . show $ quoted str
    emit (LitInt int) = integer int
    emit (LitNum num) = double num

expKeyed :: LValue -> Expression -> Expression
expKeyed = (ExpLV .) . KEYED

quoted :: String -> String
quoted = concatMap quote
    where
    quote :: Char -> String
    quote '\\' = "\\\\"
    quote x = [x]

#ifndef HADDOCK
infixl 4 <--
infixl 9 -->
infixl 4 .-
infixl 4 <-&
infixl 4 .&
#endif

{-| @.include@ directive. -}
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
(.&) :: Expression -> [Expression] -> Ins

include = DeclInc

(<:=) = InsBind
(<==) = InsAssign
(<--) = InsPrim . Just
(.-)  = InsPrim Nothing
(<-&) = InsFun
(.&)  = InsFun []

{-| Literal zero -}
lit0 :: Expression
lit0 = lit (0 :: Int)

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

instance LiteralClass [[ArgFlag]] Expression where
    lit = lit . parens . commaSep . map emit

instance LiteralClass [ArgFlag] Expression where
    lit = lit . emit

instance LiteralClass ObjType Expression where
    lit = ExpLV . VAR . render . emit

instance LiteralClass Doc Expression where
    lit = lit . render

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

{-| In the case a Perl 6 builtin corresponds exactly to a PIR opcode, you can
    use 'vop1' to create an appropriate wrapper for an opcode expecting /one/
    argument. -}
vop1 :: SubName              -- ^ Perl 6 name of the opcode to wrap
     -> PrimName             -- ^ PIR opcode
     -> Decl                 -- ^ Final subroutine declaration
vop1 p6name opname =
    sub p6name [arg0] 
      [ InsNew rv PerlScalar
      , rv <-- opname $ [arg0]
      ] --> [rv]
{-| In the case a Perl 6 builtin corresponds exactly to a PIR opcode, you can
    use 'vop2' to create an appropriate wrapper for an opcode expecting /two/
    arguments. -}
vop2 :: SubName              -- ^ Perl 6 name of the opcode to wrap
     -> PrimName             -- ^ PIR opcode
     -> Decl                 -- ^ Final subroutine declaration
vop2 p6name opname =
    sub p6name [arg0, arg1] 
      [ InsNew rv PerlScalar
      , rv <-- opname $ [arg0, arg1]
      ] --> [rv]

{-| Creates a sub which accepts a thing which allows keyed access (for
    example aggregates) and an index. -}
vop2keyed :: SubName         -- ^ Perl 6 name of the sub to create
          -> LValue          -- ^ Intermediate register to convert the index to
                             --   (e.g. 'tempINT' or 'tempSTR')
          -> Decl            -- ^ Final subroutine declaration
vop2keyed p6name temp =
    sub p6name [arg0, arg1] 
      [ temp    <:= arg1
      , rv      <:= expKeyed arg0 (ExpLV temp)
      ] --> [rv]

{-| Generic wrapper for unary opcodes. -}
--vop1x :: (RegClass a, RegClass b) => SubName -> PrimName -> a -> b -> Decl
vop1x :: SubName                     -- ^ Perl 6 name of the sub to create
      -> PrimName                    -- ^ Opcode to wrap
      -> (forall a. RegClass a => a) -- ^ Register to use for the return value
                                     --   of the op
      -> (forall b. RegClass b => b) -- ^ Register type to convert the
                                     --   parameter to
      -> Decl                        -- ^ Final subroutine declaration
vop1x p6name opname regr reg0 =
    sub p6name [arg0] 
      [ InsNew rv PerlScalar
      , reg0 <:= arg0
      , regr <-- opname $ [reg0]
      , rv   <== regr
      ] --> [rv]

{-| Generic wrapper for coercion\/context forcing (used by @&prefix:\<\+\>@,
    @&prefix:\<\~\>@, etc.) -}
vop1coerce :: SubName                     -- ^ Perl 6 name of the sub to create
           -> (forall a. RegClass a => a) -- ^ Register type to convert the
                                          --   parameter to
           -> Decl                        -- ^ Final subroutine declaration
vop1coerce p6name reg0 =
    sub p6name [arg0] 
      [ InsNew rv PerlScalar
      , reg0 <:= arg0
      , rv   <:= reg0
      ] --> [rv]

{-| Generic wrapper for two-ary opcodes. -}
vop2x :: SubName                     -- ^ Perl 6 name of the sub to create
      -> PrimName                    -- ^ Opcode to wrap
      -> (forall a. RegClass a => a) -- ^ Register to use for the return value
                                     --   of the op
      -> (forall b. RegClass b => b) -- ^ Register type to convert the
                                     --   first parameter to
      -> (forall c. RegClass c => c) -- ^ Register type to convert the
                                     --   second parameter to
      -> Decl                        -- ^ Final subroutine declaration
vop2x p6name opname regr reg0 reg1 =
    sub p6name [arg0, arg1] 
      [ InsNew rv PerlScalar
      , reg0 <:= arg0
      , reg1 <:= arg1
      , regr <-- opname $ [reg0,reg1]
      , rv   <== regr
      ] --> [rv]

{-| Wrapper for an opcode which accepts and returns an @I@ register. -}
vop1ii :: SubName -> PrimName -> Decl
vop1ii p6name opname = vop1x p6name opname tempINT tempINT
{-| Wrapper for an opcode which accepts and returns a @N@ register. -}
vop1nn :: SubName -> PrimName -> Decl
vop1nn p6name opname = vop1x p6name opname tempNUM tempNUM
{-| Wrapper for an opcode which accepts and returns a @S@ register. -}
vop1ss :: SubName -> PrimName -> Decl
vop1ss p6name opname = vop1x p6name opname tempSTR tempSTR
{-| Wrapper for an opcode which returns a @S@ register and accepts a @I@ register. -}
vop1si :: SubName -> PrimName -> Decl
vop1si p6name opname = vop1x p6name opname tempSTR tempINT
{-| Wrapper for an opcode which returns a @I@ register and accepts a @S@ register. -}
vop1is :: SubName -> PrimName -> Decl
vop1is p6name opname = vop1x p6name opname tempINT tempSTR
{-| Wrapper for an opcode which returns a @I@ register and accepts a @P@ register. -}
vop1ip :: SubName -> PrimName -> Decl
vop1ip p6name opname = vop1x p6name opname tempINT tempPMC

{-| Wrapper for an opcode which accepts and returns @I@ registers. -}
vop2iii :: SubName -> PrimName -> Decl
vop2iii p6name opname = vop2x p6name opname tempINT tempINT tempINT2 
{-| Wrapper for an opcode which accepts and returns @N@ registers. -}
vop2nnn :: SubName -> PrimName -> Decl
vop2nnn p6name opname = vop2x p6name opname tempNUM tempNUM tempNUM2 
{-| Wrapper for an opcode which accepts two @S@ registers and returns a native
    integer (@I@ register). -}
vop2iss :: SubName -> PrimName -> Decl
vop2iss p6name opname = vop2x p6name opname tempINT tempSTR tempSTR2 

bare :: VarName -> Expression
bare = ExpLV . VAR

collectCC :: [Ins]
collectCC =
    [ "set_returns" .- retSigList [tempPMC]
    , "returncc" .- []
    ]

callThunkCC :: Expression -> [Ins]
callThunkCC fun =
    [ "set_args" .- sigList []
    , "get_results" .- sigList [tempPMC]
    , "invokecc" .- [fun]
    ]

{-| Creates appropriate @&statement_control:foo@ subroutines. -}
stmtControlLoop :: VarName     -- ^ Perl 6 name of the new sub
                -> PrimName    -- ^ PIR opcode to use for branching
                -> Decl        -- ^ Final declaration of the sub
stmtControlLoop name comp = sub ("&statement_control:" ++ name) [arg0, arg1] $
    if isPost then ["goto" .- [bare redoL]] else [] ++
    [ InsLabel nextL
    , [reg tempPMC] <-& arg0 $ []
    , comp      .- [tempPMC, bare lastL]
    , InsLabel redoL
    , arg1      .& []
    , "goto"    .- [bare nextL]
    , InsLabel lastL
    , "returncc" .- []
    ]
    where
    nextL = ("sc_" ++ name ++ "_next")
    lastL = ("sc_" ++ name ++ "_last")
    redoL = ("sc_" ++ name ++ "_redo")
    isPost = "post" `isPrefixOf` name

{-| Creates appropriate @&statement_control:foo@ subroutines. -}
stmtControlCond :: VarName     -- ^ Perl 6 name of the new sub
                -> PrimName    -- ^ PIR opcode to use for branching
                -> Decl        -- ^ Final declaration of the sub
stmtControlCond name comp =
    sub ("&statement_control:" ++ name) [arg0, arg1, arg2] body --> [tempPMC]
    where
    altL = ("sc_" ++ name ++ "_alt")
    postL = ("sc_" ++ name ++ "_post")
    body = concat
        [ [ comp .- [arg0, bare altL] ]
        , callThunkCC arg1
        , [ "goto" .- [bare postL] ]
        , [ InsLabel altL ]
        , callThunkCC arg2
        , [ InsLabel postL ]
        , collectCC
        ]

{-| Creates appropriate @&infix:foo@ subs for logical operators (@||@, @&&@,
    etc.). -}
op2Logical :: VarName          -- ^ Perl 6 name of the sub to create
           -> PrimName         -- ^ PIR opcode to use (@if@, @unless@)
           -> Decl             -- ^ Final declaration of the sub
op2Logical name comp = sub ("&infix:" ++ name) [arg0, arg1] body --> [tempPMC]
    where
    altL = ("sc_" ++ escaped name ++ "_alt")
    body =
        [ comp .- [arg0, bare altL]
        , "set_returns" .- retSigList [arg0]
        , "returncc" .- []
        , InsLabel altL
        ] ++ callThunkCC arg1 ++ collectCC

{-| Escapes characters which have a special meaning in PIR. -}
escaped :: String -> String
escaped = concatMap esc
    where
    esc :: Char -> String
    esc c | isAlphaNum c = [c]
    esc c = ('_':show (ord c))

{-| The Prelude, defining primitives like @&say@, @&infix:+@, etc. -}
preludePIR :: Doc
preludePIR = emit $
    [ include "iglobals.pasm"
    , include "errors.pasm"
    -- Control flowy
    , sub "&return" [slurpy arg0]
        [ InsNew tempPMC PerlArray
        , (tempPMC `KEYED` lit False) <:= arg0
        , "throw" .- [tempPMC]
        ]
    , sub "&leave" [slurpy arg0]
        [] --> [arg0]
    , sub "&statement_control:for" [arg0, arg1]
        [ tempPMC   <-- "iter" $ [arg0]
        , InsLabel "sc_for_next"
        , "unless"  .- [tempPMC, bare "sc_for_last"]
        , tempPMC2  <-- "shift" $ [tempPMC]
        , arg1      .& [tempPMC2]
        , "goto"    .- [bare "sc_for_next"]
        , InsLabel "sc_for_last"
        , "returncc" .- []
        ]
    , sub "&statement_control:loop" [arg0, arg1, arg2, arg3]
        [ InsLabel "sc_loop_next"
        , [reg tempPMC] <-& arg1 $ []
        , "unless" .- [tempPMC, bare "sc_loop_last"]
        , arg2 .& [] -- throw away the result of body...
        , arg3 .& [] -- ...and the post-condition
        , "goto" .- [bare "sc_loop_next"]
        , InsLabel "sc_loop_last"
        , "returncc" .- []
        ]
    , stmtControlLoop "while" "unless"
    , stmtControlLoop "until" "if"
    , stmtControlCond "if" "unless"
    , stmtControlCond "unless" "if"
    , op2Logical "&&" "if"
    , op2Logical "||" "unless"
    , op2Logical "and" "if"
    , op2Logical "or" "unless"
    , sub "&nothing" [] []

    -- IO
    , sub "&print" [slurpy arg0]
        [ tempSTR <-- "join" $ [lit "", arg0]
        , "print" .- [tempSTR]
        ] --> [lit True]
    , sub "&say" [slurpy arg0]
        [ tempSTR <-- "join" $ [lit "", arg0]
        , "print" .- [tempSTR]
        , "print" .- [lit "\n"]
        ] --> [lit True]
    , vop1is "&system" "spawnw"

    -- Operators
    , sub "&infix:," [slurpy arg0]
        [] --> [arg0]
    , sub "&circumfix:[]" [slurpy arg0]
        [ InsNew rv PerlScalar
        , InsNew tempPMC PerlArray
        , tempPMC   <== arg0
        , tempPMC2  <-- "new" $ [lit PerlRef, tempPMC]
        , rv        <== tempPMC2
        ] --> [rv]
    , sub "&prefix:++" [arg0]
        [ "inc" .- [arg0]
        ] --> [arg0]
    , sub "&prefix:--" [arg0]
        [ "dec" .- [arg0]
        ] --> [arg0]
    , sub "&postfix:++" [arg0]
        [ InsNew rv PerlScalar
        , rv <== arg0
        , "inc" .- [arg0]
        ] --> [rv]
    , sub "&postfix:--" [arg0]
        [ InsNew rv PerlScalar
        , rv <== arg0
        , "dec" .- [arg0]
        ] --> [rv]
    , sub "&prefix:-" [arg0]
        [ InsNew rv PerlScalar
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
    , vop1coerce "&prefix:+" tempNUM
    , vop1coerce "&prefix:~" tempSTR
    , vop1coerce "&int"      tempINT
    , sub "&true" [arg0]
        [ InsNew rv PerlScalar
        , rv <:= (ExpLit . LitInt) 1
        , "if" .- [arg0, bare "true_pmc_is_true"]
        , rv <:= (ExpLit . LitInt) 0
        , InsLabel "true_pmc_is_true"
        ] --> [rv]

    -- Strings
    , vop1is "&chars" "length"
    , vop1is "&bytes" "bytelength"
    , sub "&prefix:\\" [arg0]
        [ tempPMC   <-- "new" $ [lit PerlRef, arg0]
        ] --> [rv]
    , sub "&infix:=>" [arg0, arg1]
        [ InsNew rv PerlPair
        , rv `KEYED` arg0 <:= arg1
        ] --> [rv]
    , sub "&infix:.." [arg0, arg1]
        [ tempINT   <:= arg0
        , InsNew rv PerlArray
        , InsLabel "range_next"
        , "lt_num"  .- [arg1, tempINT, bare "range_end"]
        , "push"    .- [rv, tempINT]
        , "inc"     .- [tempINT]
        , "goto"    .- [bare "range_next"]
        , InsLabel "range_end"
        ] --> [rv]
    , sub "&substr" [arg0, arg1, arg2]
        [ tempSTR   <:= arg0
        , tempINT   <:= arg1
        , tempINT2  <:= arg2
        , tempSTR2  <-- "substr" $ [tempSTR, tempINT, tempINT2]
        , InsNew rv PerlScalar
        , rv        <:= tempSTR2
        ] --> [rv]
    , vop1si "&chr" "chr"
    , vop1is "&ord" "ord"
    , vop2x "&infix:x" "repeat" tempSTR tempSTR tempINT
    , vop1ss "&lc" "downcase"
    , vop1ss "&uc" "upcase"

    -- Objects
    , sub "&undef" []
        [ InsNew rv PerlScalar
        ] --> [rv]
    , sub "&undefine" [arg0]
        [ InsNew tempPMC PerlScalar
        , arg0 <== tempPMC
        ] --> [arg0]
    , vop1ip "&defined" "defined"
{- XXX saying  hash
-- causes error:imcc:syntax error, unexpected IREG, expecting '('
    , sub "&id" [arg0]
        [ InsNew rv PerlScalar
        , tempINT <-- "hash" $ [arg0]
        , rv <== tempINT
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
        [ rv      <:= expKeyed arg0 arg1
        , "delete" .- [expKeyed arg0 arg1]
        ] --> [rv]
    , sub "&exists" [arg0, arg1]
        [ tempINT <-- "exists" $ [expKeyed arg0 arg1]
        , InsNew rv PerlScalar
        , rv      <:= tempINT
        ] --> [rv]
    , sub "&join" [arg0, arg1]
        [ InsNew rv PerlScalar
        , tempSTR   <:= arg0
        , tempSTR2  <-- "join" $ [tempSTR, arg1]
        , rv        <== tempSTR2
        ] --> [rv]

    , DeclNS "Perl6::Internals"
    [ sub "&symbolic_deref" [arg0, slurpy arg1]
        -- find_name($arg0 ~ join "::", @arg1)
        [ tempSTR  <-- "join" $ [lit "::", arg1]
        , tempSTR2 <:= arg0
        , tempSTR  <-- "concat" $ [tempSTR2, tempSTR]
        -- XXX: Normalise tempSTR, i.e. "&infix:<+>" -> "&infix:+"
        , rv       <-- "find_name" $ [tempSTR]
        ] --> [rv]
    , sub "&exit" [arg0]
        [ tempPMC  <-- "find_global" $ [lit "main", lit "&*END"]
        , "set_args" .- sigList []
        , "invokecc" .- [tempPMC]
        , tempINT <:= arg0
        , "exit" .- [tempINT]
        ]
    , sub "&sleep" [arg0]
        [ tempNUM <:= arg0
        , "sleep" .- [tempNUM]
        ]
    , sub "&compile_pir" [arg0]
        [ tempSTR  <:= arg0
        , tempPMC  <-- "compreg" $ [lit "PIR"]
        , tempPMC2 <-- "compile" $ [tempPMC, tempSTR]
        ] --> [tempPMC2]
    , sub "&eval_pir" [arg0]
        [ tempPMC   <-- "open" $ [lit "temp.p6", lit ">"]
        , "print"   .- [tempPMC, arg0]
        , "close"   .- [tempPMC]
        , tempPMC   <-- "open" $ [lit "pugs -CPIR temp.p6", lit "-|"]
        , InsNew rv PerlScalar
        , rv        <:= lit ""
        , InsLabel "eval_pir_read_pre_next"
        , tempSTR   <-- "readline" $ [tempPMC]
        , "ne"      .- [tempSTR, lit ".sub \"init\" :main :anon\n", bare "eval_pir_read_pre_next"]
        , InsLabel "eval_pir_read_next"
        , tempSTR   <-- "readline" $ [tempPMC]
        , "eq"      .- [tempSTR, lit ".end\n", bare "eval_pir_done"]
        , rv        <-- "concat" $ [tempSTR]
        , "if"      .- [tempPMC, bare "eval_pir_read_next"]  -- hopefully this is never false
        , InsLabel "eval_pir_done"
        , "close"   .- [tempPMC]
        ] --> [rv]
    ]
    
    -- Supporting Math::Basic
    , sub "&abs" [arg0]
        [ InsNew rv PerlScalar
        , rv    <== arg0
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
        [ InsNew rv PerlScalar
        , tempNUM  <-- "time" $ []
        , rv       <:= tempNUM
        -- Parrot's time returns seconds since 1970, but Perl 6's time
        -- returns seconds since 2000, so we've to compensate.
        , "sub" .- [rv, ExpLit . LitNum $ 946684800]
        ] --> [rv]

    --, namespace "Str"
    , sub "&split" [arg0, arg1]
        [ InsNew rv PerlScalar
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
        , tempPMC   <-- "split" $ [tempSTR, tempSTR2]
        , rv        <== tempPMC
        , InsLabel "split_done"
        ] --> [rv]

    --, namespace "bool" -- Namespaces have bugs in both pugs and parrot.
    , sub "&bool::true" []
        [] --> [lit True]
    , sub "&bool::false" []
        [] --> [lit False]
    ]

instance YAML Doc where
    asYAML = asYAML . render
instance Typeable Doc where
    typeOf _ = typeOf ()

------------------------------------------------------------------------
