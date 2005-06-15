{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Emit.PIR where
import Text.PrettyPrint
import Data.Char

data RegType = RegInt | RegNum | RegStr | RegPMC
    deriving (Show, Eq)

data RelOp = RelLT | RelLE | RelEQ | RelNE | RelGE | RelGT
    deriving (Show, Eq)

data ObjType    = PerlUndef
    deriving (Show, Eq)

type LabelName  = String
type SubName    = String
type VarName    = String
type PrimName   = String
type PkgName    = String

data LValue
    = VAR !VarName
    | PMC !Int
    | STR !Int
    | INT !Int
    deriving (Show, Eq)

data Expression
    = ExpLV !LValue
    | ExpLit !Literal
    deriving (Show, Eq)

data Literal
    = LitStr !String
    | LitInt !Integer
    | LitNum !Double
    deriving (Show, Eq)

class (Show x) => Emit x where
    emit :: x -> Doc
    -- emit x = error ("Unrecognized construct: " ++ show x)

instance Emit String where
    emit = text

instance Emit Decl where
    emit (DeclNS name) = emit ".namespace" <+> brackets (quotes $ emit name)
    emit (DeclSub name styps stmts)
        =  (emit ".sub" <+> doubleQuotes (emit name) <+> commaSep styps)
        $+$ nested (emitBody stmts)
        $+$ emit ".end"
        where
        emitBody [] = []
        emitBody [(StmtIns (InsFun _ name args))] = [emit $ StmtIns (InsTailFun name args)]
        emitBody (x:xs) = emit x : emitBody xs

instance Emit SubType where
    emit = emit . ('@':) . drop 3 . show

instance (Emit a) => Emit [a] where
    emit = vcat . map emit

nested :: (Emit x) => x -> Doc
nested = nest 4 . emit

eqSep :: (Emit a, Emit b, Emit c) => a -> b -> [c] -> Doc
eqSep lhs rhs args = emit lhs <+> equals <+> emit rhs <+> commaSep args

commaSep :: (Emit x) => [x] -> Doc
commaSep = hsep . punctuate comma . map emit

curPad :: Doc
curPad = int (-1)

instance Emit Stmt where
    emit (StmtComment []) = empty
    emit (StmtComment str) = char '#' <+> emit str
    emit (StmtLine file line) = text "#line" <+> doubleQuotes (emit file) <+> emit line
    emit (StmtIns ins) = emit ins
    emit (StmtLabel name ins) = emit name <> colon $+$ emit ins
    emit (StmtPad pad stmts) = vcat $
        [ emit "new_pad" <+> curPad
        ] ++ map (\(var, exp) -> emit ("store_lex" .- [lit (-1 :: Int), lit var, exp])) pad

instance Emit RegType where
    emit = emit . map toLower . drop 3 . show

instance Emit Ins where
    emit (InsLocal rtyp name) = emit ".local" <+> emit rtyp <+> emit name
    emit (InsNew ident otyp) = eqSep ident "new" [otyp]
    emit (InsAssign ident lit) = eqSep ident "assign" [lit]
    emit (InsPrim (Just ret) name args) = eqSep ret name args
    emit (InsPrim Nothing name args) = emit name <+> commaSep args
    emit (InsFun rets (ExpLit (LitStr name)) args) = emitRets rets
        $+$ emitFunName "invokecc" name args
    emit (InsFun rets fun args) = emitRets rets
        $+$ emitFun "invokecc" fun args
    emit (InsTailFun (ExpLit (LitStr name)) args) = emitFunName "tailcall" name args
    emit (InsExp exp) = empty
    emit x = error $ "can't emit ins: " ++ show x

instance Emit Doc where
    emit = id

emitRets [] = empty
emitRets rets = emit ("get_results" .- sigList rets)

emitFun callconv fun args = vcat
    [ emit "set_args" <+> commaSep (sig:map emit args)
    , emit callconv <+> emit fun
    ]
    where
    sig = quotes $ parens (commaSep (replicate (length args) "0b10010"))

emitFunName :: (Emit a, Emit b) => a -> String -> [b] -> Doc
emitFunName callconv name args = eqSep (funPMC :: LValue) "find_name" [LitStr name]
    $+$ emitFun callconv (funPMC :: LValue) args

noArgs :: [Expression]
noArgs = []

-- set_args '(0b0,0b0,0b0)', $P1, $P2, $P3

instance Emit ObjType where
    emit = emit . ('.':) . show

instance Emit Expression where
    emit (ExpLV lhs) = emit lhs
    emit (ExpLit lit) = emit lit

instance Emit LValue where
    emit (VAR name) = emit name
    emit (PMC num) = emit "$P" <> emit num
    emit (STR str) = emit "$S" <> emit str
    emit (INT str) = emit "$I" <> emit str

instance Emit Literal where
    emit (LitStr str) = text . show $ concatMap quoted str
        where
        quoted :: Char -> String
        quoted '\'' = "\\'"
        quoted '\\' = "\\\\"
        quoted x = [x]
    emit (LitInt int) = integer int
    emit (LitNum num) = double num

instance Emit Int where
    emit = int

data Stmt
    = StmtComment   !String
    | StmtLine      !FilePath !Int
    | StmtIns       !Ins
    | StmtLabel     !LabelName !Ins
    | StmtPad       ![(VarName, Expression)] ![Stmt]
    deriving (Show, Eq)

data Ins
    = InsLocal      !RegType !VarName
    | InsNew        !LValue !ObjType 
    | InsBind       !LValue !Expression
    | InsAssign     !LValue !Expression
    | InsExp        !Expression
    | InsFun        ![Sig] !Expression ![Expression]
    | InsTailFun    !Expression ![Expression]
    | InsPrim       !(Maybe LValue) !PrimName ![Expression]
    deriving (Show, Eq)

data SubType = SubMAIN | SubLOAD | SubANON | SubMETHOD | SubMULTI [ObjType]
    deriving (Show, Eq)

type PIR = [Decl]

data Decl
    = DeclSub   !SubName ![SubType] ![Stmt]
    | DeclNS    !PkgName
    deriving (Show, Eq)

{-|
  <-- is calling an opcode, that returns a value.
  .-  is calling an opcode, with any return value ignored.
  <-& is calling a user-defined function, that returns a list of values.
 .&   is calling a user-defined function, with any return value ignored.
 -->  is returning from a sub.
-}
infixl 4 <--
infixl 9 -->
infixl 4 .-
infixl 4 <-&
infixl 4 .&

namespace = DeclNS
(<--) = InsPrim . Just
(.-)  = InsPrim Nothing
(<-&) = InsFun
(.&)  = InsFun [] . lit

nullPMC :: (RegClass a) => a
nullPMC = reg $ PMC 0

funPMC :: (RegClass a) => a
funPMC = reg $ PMC 1

outPMC :: (RegClass a) => a
outPMC = reg $ PMC 8

tempPMC :: (RegClass a) => a
tempPMC = reg $ PMC 9

tempSTR :: (RegClass a) => a
tempSTR = reg $ STR 9

tempINT :: (RegClass a) => a
tempINT = reg $ INT 9

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

sub :: SubName -> [Sig] -> [Ins] -> Decl
sub name [] body = DeclSub name [] (map StmtIns body)
sub name sigs body = DeclSub name [] stmts
    where
    param = "get_params" .- sigList sigs
    stmts = map StmtIns (param:body)

sigList sigs = (flags:map sigIdent sigs)
    where
    flags = lit . render . parens . commaSep $ map sigFlags sigs

instance Emit [SigFlag] where
    emit [MkFlagSlurpy] = emit "0b1010"
    emit [] = emit "0b0"
    emit _ = error "Unknown sig"

data Sig = MkSig
    { sigFlags  :: [SigFlag]
    , sigIdent  :: Expression
    }
    deriving (Show, Eq)
data SigFlag = MkFlagSlurpy
    deriving (Show, Eq)

slurpy :: Expression -> Sig
slurpy = MkSig [MkFlagSlurpy]

(-->) :: Decl -> [Expression] -> Decl
(DeclSub name styps stmts) --> rets = DeclSub name styps $ stmts ++ map StmtIns
    [ "set_returns" .- (lit sig : rets)
    , "returncc" .- []
    ]
    where
    sig = parens (commaSep (replicate (length rets) "0b10010"))
_ --> _ = error "Can't return from non-sub"

preludePIR :: Doc
preludePIR = emit $
    [ sub "&print" [slurpy tempPMC]
        [ tempSTR <-- "join" $ [lit "", tempPMC]
        , "print" .- [tempSTR]
        ] --> [lit True]
    , sub "&say" [slurpy tempPMC]
        [ "push" .- [tempPMC, lit "\n"]
        , "&print" .& [tempPMC]
        ]
    , sub "&pop" [tempPMC]
        [ outPMC <-- "pop" $ [tempPMC]
        ] --> [outPMC]
    , sub "&prefix:++" [tempPMC]
        [ "inc" .- [tempPMC]
        ] --> [tempPMC]
    , sub "&postfix:++" [tempPMC]
        [ InsNew outPMC PerlUndef
        , outPMC <-- "assign" $ [tempPMC]
        , "inc" .- [tempPMC]
        ] --> [outPMC]
    , sub "&nothing" [] []
    , namespace "bool"
    , sub "&true" []
        [] --> [lit True]
    , sub "&false" []
        [] --> [lit False]
    , namespace "main"
    ]

