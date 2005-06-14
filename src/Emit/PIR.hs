{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Emit.PIR where
import Text.PrettyPrint
import Pugs.Internals
import Pugs.Pretty

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

data Identifier
    = VAR VarName
    | PMC Int
    deriving (Show, Eq)

data Literal
    = LitStr String
    | LitInt Integer
    | LitNum Double
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
        $+$ nested stmts
        $+$ emit ".end"

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
    emit (StmtPad stmts) = emit "new_pad" <+> curPad

instance Emit RegType where
    emit = emit . map toLower . drop 3 . show

instance Emit Ins where
    emit (InsLocal rtyp name) = emit ".local" <+> emit rtyp <+> emit name
    emit (InsNew ident otyp) = eqSep ident "new" [otyp]
    emit (InsAssign ident lit) = eqSep ident lit noArgs
    emit (InsFun rets name args) = vcat
        [ eqSep (PMC 0) "find_name" [LitStr name]
        , emit "set_args" <+> sig <> comma <+> commaSep args
        , emit "invokecc" <+> emit (PMC 0)
        ]
        where
        sig = quotes $ parens (commaSep (replicate (length args) "0"))
    emit x = error $ "can't emit: " ++ show x

noArgs :: [Identifier]
noArgs = []

-- set_args '(0b0,0b0,0b0)', $P1, $P2, $P3

instance Emit ObjType where
    emit = emit . ('.':) . show

instance Emit Identifier where
    emit (VAR name) = emit name
    emit (PMC num) = emit "$P" <> emit num

instance Emit Literal where
    emit (LitStr str) = text . show $ encodeUTF8 (concatMap quoted str)

instance Emit Int where
    emit = int

data Stmt
    = StmtComment   String
    | StmtLine      FilePath Int
    | StmtIns       Ins
    | StmtLabel     LabelName Ins
    | StmtPad       [Stmt]
    deriving (Show, Eq)

data Ins
    = InsLocal      RegType VarName
    | InsNew        Identifier ObjType 
    | InsBind       Identifier Identifier
    | InsAssign     Identifier Literal
    | InsFun        [Identifier] PrimName [Identifier]
    | InsPrim       (Maybe Identifier) PrimName [Identifier]
    | InsStoreLex   VarName Identifier
    deriving (Show, Eq)

data SubType = SubMAIN | SubLOAD | SubANON | SubMETHOD | SubMULTI [ObjType]
    deriving (Show, Eq)

type PIR = [Decl]

data Decl
    = DeclSub   SubName [SubType] [Stmt]
    | DeclNS    PkgName
    deriving (Show, Eq)

{-
.sub main @MAIN
    .local pmc s__k

.end
    Comment :: String -> PIR Stmt
    Stmt    :: Maybe Label -> PIR Ins -> PIR Stmt
    RegHard :: RegType -> Int -> PIR Var
    RegTemp :: RegType -> Int -> PIR Var
    Ident   :: Ident -> PIR Var
    Lit     :: (ConstClass a) => a -> PIR Constant
    Pragma  :: Pragma -> PIR Decl
    Sub     :: Ident -> [PIR Parameter] -> [PIR Stmt] -> PIR Decl
    Emit    :: [PIR PASM] -> PIR Decl
    TypReg  :: RegType -> PIR Typ
    TypPMC  :: Ident -> PIR Typ
    PASM    :: [String] -> PIR PASM
    Param   :: PIR Var -> PIR Parameter
    Return  :: [PIR Var] -> PIR Stmt
    Local   :: PIR Typ -> [Ident] -> PIR Stmt
    Const   :: PIR Typ -> Ident -> PIR Constant -> PIR Stmt
    NS      :: Ident -> [PIR Decl] -> PIR Decl
    Goto    :: Ident -> PIR Stmt
    Cond    :: Bool -> PIR Var -> Ident -> PIR Stmt
    CondCmp :: Bool -> PIR Var -> RelOP -> PIR Var -> Ident -> PIR Stmt
-}

preludePIR :: Doc
preludePIR = vcat . map emit $
    [ ".sub \"&print\""
    , "    get_params '(0b1000)', $P0"
    , "    $S0 = join '', $P0"
    , "    print $S0"
    , ".end"
    , ""
    , ".sub \"&say\""
    , "    get_params '(0b1000)', $P0"
    , "    $P1 = find_name '&print'"
    , "    set_args '(0b1000)', $P0"
    , "    invokecc $P1"
    , "    print \"\\n\""
    , ".end"
    , ""
    , ".namespace ['main']"
    , ""
    ]
