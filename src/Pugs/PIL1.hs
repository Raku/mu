{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fno-warn-orphans #-}

module Pugs.PIL1 where
import Pugs.AST hiding (Prim)
import Pugs.Internals
import Pugs.Types
import Emit.PIR
import DrIFT.Perl5
import Text.XML.HaXml.Haskell2Xml

{-! global : Perl5, Haskell2Xml !-}

instance Perl5 VInt where 
    showPerl5 = show
instance Perl5 VRat where 
    showPerl5 r = "(" ++ show x ++ "/" ++ show y ++ ")"
        where
        x = numerator r
        y = denominator r
instance Perl5 VNum where 
    showPerl5 = show
instance (Typeable a) => Perl5 (TVar a) where
    showPerl5 _ = "(warn '<ref>')"
instance Perl5 Exp where
    showPerl5 _ = "(undef)"

instance (Typeable a) => Haskell2Xml (TVar a) where
    toHType   _    = Prim "TVar" "tvar"
    toContents _   = [CElem (Elem "tvar" [] [])]

instance Haskell2Xml Exp where
    toHType   _    = Prim "Exp" "exp"
    toContents _   = [CElem (Elem "exp" [] [])]
    fromContents _ = (Noop, [])

instance Haskell2Xml VRat where
    toHType   _    = Prim "Rational" "rational"
    toContents i   = [CElem (Elem "rational" [mkAttr "value" (show i)] [])]
        where
        mkAttr :: String -> String -> Attribute
        mkAttr n v = (n, AttValue [Left v])
    fromContents (CElem (Elem "rational" [("value",(AttValue [Left s]))] []):cs)
                   = (read s, cs)
    fromContents (_:cs) = fromContents cs

{-|
    The plan here is to first compile the environment (subroutines,
    statements, etc.) to an abstract syntax tree ('PIL' -- Pugs Intermediate
    Language) using the 'compile' function and 'Compile' class.
-}

data PIL_Environment = PIL_Environment
    { pilMain :: [PIL_Decl]
    , pilGlob :: PIL_Stmts
    }
    deriving (Show, Eq, Ord, Typeable)

data PIL_Stmts = PNil
    | PStmts
        { pStmt  :: !PIL_Stmt
        , pStmts :: !PIL_Stmts
        }
    | PPad
        { pScope :: !Scope
        , pSyms  :: ![(VarName, PIL_Expr)]
        , pStmts :: !PIL_Stmts
        }
    deriving (Show, Eq, Ord, Typeable)

data PIL_Stmt = PNoop | PStmt { pExpr :: !PIL_Expr } | PPos
        { pPos  :: !Pos
        , pExp  :: !Exp
        , pNode :: !PIL_Stmt
        }
    deriving (Show, Eq, Ord, Typeable)

data PIL_Expr
    = PRawName { pRawName :: !VarName }
    | PExp { pLV  :: !PIL_LValue }
    | PLit { pLit :: !PIL_Literal }
    | PThunk { pThunk :: !PIL_Expr }
    | PCode
        { pType    :: !SubType
        , pParams  :: ![TParam]
        , pBody    :: !PIL_Stmts
        }
    deriving (Show, Eq, Ord, Typeable)

data PIL_Decl = PSub
    { pSubName      :: !SubName
    , pSubType      :: !SubType
    , pSubParams    :: ![TParam]
    , pSubBody      :: !PIL_Stmts
    }
    deriving (Show, Eq, Ord, Typeable)

data PIL_Literal = PVal { pVal :: Val }
    deriving (Show, Eq, Ord, Typeable)

data PIL_LValue = PVar { pVarName :: !VarName }
    | PApp 
        { pCxt  :: !TCxt
        , pFun  :: !PIL_Expr
        , pInv  :: !(Maybe PIL_Expr)
        , pArgs :: ![PIL_Expr]
        }
    | PAssign
        { pLHS  :: ![PIL_LValue]
        , pRHS  :: !PIL_Expr
        }
    | PBind
        { pLHS  :: ![PIL_LValue]
        , pRHS  :: !PIL_Expr
        }
    deriving (Show, Eq, Ord, Typeable)

data TParam = MkTParam
    { tpParam   :: !Param
    , tpDefault :: !(Maybe (PIL_Expr))
    }
    deriving (Show, Eq, Ord, Typeable)

data TCxt
    = TCxtVoid | TCxtLValue !Type | TCxtItem !Type | TCxtSlurpy !Type
    | TTailCall !TCxt
    deriving (Show, Eq, Ord, Typeable)

data TEnv = MkTEnv
    { tLexDepth :: !Int                 -- ^ Lexical scope depth
    , tTokDepth :: !Int                 -- ^ Exp nesting depth
    , tCxt      :: !TCxt                -- ^ Current context
    , tReg      :: !(TVar (Int, String))-- ^ Register name supply
    , tLabel    :: !(TVar Int)          -- ^ Label name supply
    }
    deriving (Show, Eq, Ord, Typeable)

------------------------------------------------------------------------
{-

data Scope = SState  -- ^ Persistent across calls
           | SMy     -- ^ Lexical
           | SOur    -- ^ Package
           | SLet    -- ^ Hypotheticalised (reverted upon failure)
           | STemp   -- ^ Temporary (reverted at scope exit)
           | SGlobal -- ^ Global

data SubType = SubMethod    -- ^ Method
             | SubCoroutine -- ^ Coroutine
             | SubMacro     -- ^ Macro
             | SubRoutine   -- ^ Regular subroutine
             | SubBlock     -- ^ Bare block
             | SubPointy    -- ^ Pointy sub
             | SubPrim      -- ^ Built-in primitive operator (see "Pugs.Prim")


data Val
    = VUndef                 -- ^ Undefined value
    | VBool     !VBool       -- ^ Boolean value
    | VInt      !VInt        -- ^ Integer value
    | VRat      !VRat        -- ^ Rational number value
    | VNum      !VNum        -- ^ Number (i.e. a double)
    | VStr      !VStr        -- ^ String value
    | VList     !VList       -- ^ List value
    | VType     !VType       -- ^ Type value (e.g. @Int@ or @Type@)

data Cxt = CxtVoid         -- ^ Context that isn't expecting any values
         | CxtItem !Type   -- ^ Context expecting a value of the specified type
         | CxtSlurpy !Type -- ^ Context expecting multiple values of the
                           --     specified type
data Type
    = MkType !String      -- ^ A regular type
    | TypeOr  !Type !Type -- ^ The disjunction (|) of two types
    | TypeAnd !Type !Type -- ^ The conjunction (&) of two types

data Param = MkParam
    { isInvocant    :: !Bool        -- ^ Is it in invocant slot?
    , isOptional    :: !Bool        -- ^ Is it optional?
    , isNamed       :: !Bool        -- ^ Is it named-only?
    , isLValue      :: !Bool        -- ^ Is it lvalue (i.e. not `is copy`)?
    , isWritable    :: !Bool        -- ^ Is it writable (i.e. `is rw`)?
    , isLazy        :: !Bool        -- ^ Is it call-by-name (short-circuit)?
    , paramName     :: !String      -- ^ Parameter name
    , paramContext  :: !Cxt         -- ^ Parameter context: slurpiness and type
    , paramDefault  :: !Exp         -- ^ Default expression (to evaluate to)
    }

data Pos = MkPos
    { posName           :: !String
    , posBeginLine      :: !Int
    , posBeginColumn    :: !Int
    , posEndLine        :: !Int
    , posEndColumn      :: !Int
    }

-}

{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Perl5 PIL_Environment where
    showPerl5 (PIL_Environment aa ab) =
	      showP5HashObj "PIL::Environment"
	      [("pilMain", showPerl5 aa) , ("pilGlob", showPerl5 ab)]

instance Haskell2Xml PIL_Environment where
    toHType v =
	Defined "PIL_Environment" []
		[Constr "PIL_Environment" [] [toHType aa,toHType ab]]
      where
	(PIL_Environment aa ab) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PIL_Environment" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,_)-> (PIL_Environment aa ab, etc))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(PIL_Environment aa ab) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab])]

instance Perl5 PIL_Stmts where
    showPerl5 (PNil) = showP5Class "PNil"
    showPerl5 (PStmts aa ab) = showP5HashObj "PStmts"
	      [("pStmt", showPerl5 aa) , ("pStmts", showPerl5 ab)]
    showPerl5 (PPad aa ab ac) = showP5HashObj "PPad"
	      [("pScope", showPerl5 aa) , ("pSyms", showPerl5 ab) ,
	       ("pStmts", showPerl5 ac)]

instance Haskell2Xml PIL_Stmts where
    toHType v =
	Defined "PIL_Stmts" []
		[Constr "PNil" [] [],Constr "PStmts" [] [toHType aa,toHType ab],
		 Constr "PPad" [] [toHType ac,toHType ad,toHType ae]]
      where
	(PStmts aa ab) = v
	(PPad ac ad ae) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PStmts" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,_)-> (PStmts aa ab, etc)) (fromContents cs00))
	    (fromContents cs)
	| "PPad" `isPrefixOf` constr =
	    (\(ac,cs00)-> (\(ad,cs01)-> (\(ae,_)-> (PPad ac ad ae, etc))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
	| "PNil" `isPrefixOf` constr =
	    (PNil,etc)
    toContents v@PNil =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@(PStmts aa ab) =
	[mkElemC (showConstr 1 (toHType v)) (concat [toContents aa,
						     toContents ab])]
    toContents v@(PPad ac ad ae) =
	[mkElemC (showConstr 2 (toHType v)) (concat [toContents ac,
						     toContents ad,toContents ae])]

instance Perl5 PIL_Stmt where
    showPerl5 (PNoop) = showP5Class "PNoop"
    showPerl5 (PStmt aa) = showP5HashObj "PStmt"
	      [("pExpr", showPerl5 aa)]
    showPerl5 (PPos aa ab ac) = showP5HashObj "PPos"
	      [("pPos", showPerl5 aa) , ("pExp", showPerl5 ab) ,
	       ("pNode", showPerl5 ac)]

instance Haskell2Xml PIL_Stmt where
    toHType v =
	Defined "PIL_Stmt" []
		[Constr "PNoop" [] [],Constr "PStmt" [] [toHType aa],
		 Constr "PPos" [] [toHType ab,toHType ac,toHType ad]]
      where
	(PStmt aa) = v
	(PPos ab ac ad) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PStmt" `isPrefixOf` constr =
	    (\(aa,_)-> (PStmt aa, etc)) (fromContents cs)
	| "PPos" `isPrefixOf` constr =
	    (\(ab,cs00)-> (\(ac,cs01)-> (\(ad,_)-> (PPos ab ac ad, etc))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
	| "PNoop" `isPrefixOf` constr =
	    (PNoop,etc)
    toContents v@PNoop =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@(PStmt aa) =
	[mkElemC (showConstr 1 (toHType v)) (toContents aa)]
    toContents v@(PPos ab ac ad) =
	[mkElemC (showConstr 2 (toHType v)) (concat [toContents ab,
						     toContents ac,toContents ad])]

instance Perl5 PIL_Expr where
    showPerl5 (PRawName aa) = showP5HashObj "PRawName"
	      [("pRawName", showPerl5 aa)]
    showPerl5 (PExp aa) = showP5HashObj "PExp" [("pLV", showPerl5 aa)]
    showPerl5 (PLit aa) = showP5HashObj "PLit" [("pLit", showPerl5 aa)]
    showPerl5 (PThunk aa) = showP5HashObj "PThunk"
	      [("pThunk", showPerl5 aa)]
    showPerl5 (PCode aa ab ac) = showP5HashObj "PCode"
	      [("pType", showPerl5 aa) , ("pParams", showPerl5 ab) ,
	       ("pBody", showPerl5 ac)]

instance Haskell2Xml PIL_Expr where
    toHType v =
	Defined "PIL_Expr" []
		[Constr "PRawName" [] [toHType aa],Constr "PExp" [] [toHType ab],
		 Constr "PLit" [] [toHType ac],Constr "PThunk" [] [toHType ad],
		 Constr "PCode" [] [toHType ae,toHType af,toHType ag]]
      where
	(PRawName aa) = v
	(PExp ab) = v
	(PLit ac) = v
	(PThunk ad) = v
	(PCode ae af ag) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PThunk" `isPrefixOf` constr =
	    (\(ad,_)-> (PThunk ad, etc)) (fromContents cs)
	| "PRawName" `isPrefixOf` constr =
	    (\(aa,_)-> (PRawName aa, etc)) (fromContents cs)
	| "PLit" `isPrefixOf` constr =
	    (\(ac,_)-> (PLit ac, etc)) (fromContents cs)
	| "PExp" `isPrefixOf` constr =
	    (\(ab,_)-> (PExp ab, etc)) (fromContents cs)
	| "PCode" `isPrefixOf` constr =
	    (\(ae,cs00)-> (\(af,cs01)-> (\(ag,_)-> (PCode ae af ag, etc))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(PRawName aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(PExp ab) =
	[mkElemC (showConstr 1 (toHType v)) (toContents ab)]
    toContents v@(PLit ac) =
	[mkElemC (showConstr 2 (toHType v)) (toContents ac)]
    toContents v@(PThunk ad) =
	[mkElemC (showConstr 3 (toHType v)) (toContents ad)]
    toContents v@(PCode ae af ag) =
	[mkElemC (showConstr 4 (toHType v)) (concat [toContents ae,
						     toContents af,toContents ag])]

instance Perl5 PIL_Decl where
    showPerl5 (PSub aa ab ac ad) = showP5HashObj "PSub"
	      [("pSubName", showPerl5 aa) , ("pSubType", showPerl5 ab) ,
	       ("pSubParams", showPerl5 ac) , ("pSubBody", showPerl5 ad)]

instance Haskell2Xml PIL_Decl where
    toHType v =
	Defined "PIL_Decl" []
		[Constr "PSub" [] [toHType aa,toHType ab,toHType ac,toHType ad]]
      where
	(PSub aa ab ac ad) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PSub" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,cs02)-> (\(ad,
							 _)-> (PSub aa ab ac ad, etc))
						      (fromContents cs02))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(PSub aa ab ac ad) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab,toContents ac,toContents ad])]

instance Perl5 PIL_Literal where
    showPerl5 (PVal aa) = showP5HashObj "PVal" [("pVal", showPerl5 aa)]

instance Haskell2Xml PIL_Literal where
    toHType v =
	Defined "PIL_Literal" [] [Constr "PVal" [] [toHType aa]]
      where
	(PVal aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PVal" `isPrefixOf` constr =
	    (\(aa,_)-> (PVal aa, etc)) (fromContents cs)
    toContents v@(PVal aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance Perl5 PIL_LValue where
    showPerl5 (PVar aa) = showP5HashObj "PVar"
	      [("pVarName", showPerl5 aa)]
    showPerl5 (PApp aa ab ac ad) = showP5HashObj "PApp"
	      [("pCxt", showPerl5 aa) , ("pFun", showPerl5 ab) ,
	       ("pInv", showPerl5 ac) , ("pArgs", showPerl5 ad)]
    showPerl5 (PAssign aa ab) = showP5HashObj "PAssign"
	      [("pLHS", showPerl5 aa) , ("pRHS", showPerl5 ab)]
    showPerl5 (PBind aa ab) = showP5HashObj "PBind"
	      [("pLHS", showPerl5 aa) , ("pRHS", showPerl5 ab)]

instance Haskell2Xml PIL_LValue where
    toHType v =
	Defined "PIL_LValue" []
		[Constr "PVar" [] [toHType aa],
		 Constr "PApp" [] [toHType ab,toHType ac,toHType ad,toHType ae],
		 Constr "PAssign" [] [toHType af,toHType ag],
		 Constr "PBind" [] [toHType ah,toHType ai]]
      where
	(PVar aa) = v
	(PApp ab ac ad ae) = v
	(PAssign af ag) = v
	(PBind ah ai) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "PVar" `isPrefixOf` constr =
	    (\(aa,_)-> (PVar aa, etc)) (fromContents cs)
	| "PBind" `isPrefixOf` constr =
	    (\(ah,cs00)-> (\(ai,_)-> (PBind ah ai, etc)) (fromContents cs00))
	    (fromContents cs)
	| "PAssign" `isPrefixOf` constr =
	    (\(af,cs00)-> (\(ag,_)-> (PAssign af ag, etc)) (fromContents cs00))
	    (fromContents cs)
	| "PApp" `isPrefixOf` constr =
	    (\(ab,cs00)-> (\(ac,cs01)-> (\(ad,cs02)-> (\(ae,
							 _)-> (PApp ab ac ad ae, etc))
						      (fromContents cs02))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(PVar aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(PApp ab ac ad ae) =
	[mkElemC (showConstr 1 (toHType v)) (concat [toContents ab,
						     toContents ac,toContents ad,toContents ae])]
    toContents v@(PAssign af ag) =
	[mkElemC (showConstr 2 (toHType v)) (concat [toContents af,
						     toContents ag])]
    toContents v@(PBind ah ai) =
	[mkElemC (showConstr 3 (toHType v)) (concat [toContents ah,
						     toContents ai])]

instance Perl5 TParam where
    showPerl5 (MkTParam aa ab) = showP5HashObj "MkTParam"
	      [("tpParam", showPerl5 aa) , ("tpDefault", showPerl5 ab)]

instance Haskell2Xml TParam where
    toHType v =
	Defined "TParam" [] [Constr "MkTParam" [] [toHType aa,toHType ab]]
      where
	(MkTParam aa ab) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "MkTParam" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,_)-> (MkTParam aa ab, etc))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(MkTParam aa ab) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab])]

instance Perl5 TCxt where
    showPerl5 (TCxtVoid) = showP5Class "TCxtVoid"
    showPerl5 (TCxtLValue aa) = showP5ArrayObj "TCxtLValue"
	      [showPerl5 aa]
    showPerl5 (TCxtItem aa) = showP5ArrayObj "TCxtItem" [showPerl5 aa]
    showPerl5 (TCxtSlurpy aa) = showP5ArrayObj "TCxtSlurpy"
	      [showPerl5 aa]
    showPerl5 (TTailCall aa) = showP5ArrayObj "TTailCall"
	      [showPerl5 aa]

instance Haskell2Xml TCxt where
    toHType v =
	Defined "TCxt" []
		[Constr "TCxtVoid" [] [],Constr "TCxtLValue" [] [toHType aa],
		 Constr "TCxtItem" [] [toHType ab],
		 Constr "TCxtSlurpy" [] [toHType ac],
		 Constr "TTailCall" [] [toHType ad]]
      where
	(TCxtLValue aa) = v
	(TCxtItem ab) = v
	(TCxtSlurpy ac) = v
	(TTailCall ad) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "TTailCall" `isPrefixOf` constr =
	    (\(ad,_)-> (TTailCall ad, etc)) (fromContents cs)
	| "TCxtVoid" `isPrefixOf` constr =
	    (TCxtVoid,etc)
	| "TCxtSlurpy" `isPrefixOf` constr =
	    (\(ac,_)-> (TCxtSlurpy ac, etc)) (fromContents cs)
	| "TCxtLValue" `isPrefixOf` constr =
	    (\(aa,_)-> (TCxtLValue aa, etc)) (fromContents cs)
	| "TCxtItem" `isPrefixOf` constr =
	    (\(ab,_)-> (TCxtItem ab, etc)) (fromContents cs)
    toContents v@TCxtVoid =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@(TCxtLValue aa) =
	[mkElemC (showConstr 1 (toHType v)) (toContents aa)]
    toContents v@(TCxtItem ab) =
	[mkElemC (showConstr 2 (toHType v)) (toContents ab)]
    toContents v@(TCxtSlurpy ac) =
	[mkElemC (showConstr 3 (toHType v)) (toContents ac)]
    toContents v@(TTailCall ad) =
	[mkElemC (showConstr 4 (toHType v)) (toContents ad)]

instance Perl5 TEnv where
    showPerl5 (MkTEnv aa ab ac ad ae) = showP5HashObj "MkTEnv"
	      [("tLexDepth", showPerl5 aa) , ("tTokDepth", showPerl5 ab) ,
	       ("tCxt", showPerl5 ac) , ("tReg", showPerl5 ad) ,
	       ("tLabel", showPerl5 ae)]

instance Haskell2Xml TEnv where
    toHType v =
	Defined "TEnv" []
		[Constr "MkTEnv" []
			[toHType aa,toHType ab,toHType ac,toHType ad,toHType ae]]
      where
	(MkTEnv aa ab ac ad ae) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "MkTEnv" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,cs02)-> (\(ad,cs03)-> (\(ae,
								       _)-> (MkTEnv aa ab ac ad ae, etc))
								    (fromContents cs03))
						      (fromContents cs02))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(MkTEnv aa ab ac ad ae) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab,toContents ac,toContents ad,
						     toContents ae])]

instance Perl5 Scope where
    showPerl5 (SState) = showP5Class "SState"
    showPerl5 (SMy) = showP5Class "SMy"
    showPerl5 (SOur) = showP5Class "SOur"
    showPerl5 (SLet) = showP5Class "SLet"
    showPerl5 (STemp) = showP5Class "STemp"
    showPerl5 (SGlobal) = showP5Class "SGlobal"

instance Haskell2Xml Scope where
    toHType v =
	Defined "Scope" []
		[Constr "SState" [] [],Constr "SMy" [] [],Constr "SOur" [] [],
		 Constr "SLet" [] [],Constr "STemp" [] [],Constr "SGlobal" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
	| "STemp" `isPrefixOf` constr =
	    (STemp,etc)
	| "SState" `isPrefixOf` constr =
	    (SState,etc)
	| "SOur" `isPrefixOf` constr =
	    (SOur,etc)
	| "SMy" `isPrefixOf` constr =
	    (SMy,etc)
	| "SLet" `isPrefixOf` constr =
	    (SLet,etc)
	| "SGlobal" `isPrefixOf` constr =
	    (SGlobal,etc)
    toContents v@SState =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@SMy =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@SOur =
	[mkElemC (showConstr 2 (toHType v)) []]
    toContents v@SLet =
	[mkElemC (showConstr 3 (toHType v)) []]
    toContents v@STemp =
	[mkElemC (showConstr 4 (toHType v)) []]
    toContents v@SGlobal =
	[mkElemC (showConstr 5 (toHType v)) []]

instance Perl5 SubType where
    showPerl5 (SubMethod) = showP5Class "SubMethod"
    showPerl5 (SubCoroutine) = showP5Class "SubCoroutine"
    showPerl5 (SubMacro) = showP5Class "SubMacro"
    showPerl5 (SubRoutine) = showP5Class "SubRoutine"
    showPerl5 (SubBlock) = showP5Class "SubBlock"
    showPerl5 (SubPointy) = showP5Class "SubPointy"
    showPerl5 (SubPrim) = showP5Class "SubPrim"

instance Haskell2Xml SubType where
    toHType v =
	Defined "SubType" []
		[Constr "SubMethod" [] [],Constr "SubCoroutine" [] [],
		 Constr "SubMacro" [] [],Constr "SubRoutine" [] [],
		 Constr "SubBlock" [] [],Constr "SubPointy" [] [],
		 Constr "SubPrim" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
	| "SubRoutine" `isPrefixOf` constr =
	    (SubRoutine,etc)
	| "SubPrim" `isPrefixOf` constr =
	    (SubPrim,etc)
	| "SubPointy" `isPrefixOf` constr =
	    (SubPointy,etc)
	| "SubMethod" `isPrefixOf` constr =
	    (SubMethod,etc)
	| "SubMacro" `isPrefixOf` constr =
	    (SubMacro,etc)
	| "SubCoroutine" `isPrefixOf` constr =
	    (SubCoroutine,etc)
	| "SubBlock" `isPrefixOf` constr =
	    (SubBlock,etc)
    toContents v@SubMethod =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@SubCoroutine =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@SubMacro =
	[mkElemC (showConstr 2 (toHType v)) []]
    toContents v@SubRoutine =
	[mkElemC (showConstr 3 (toHType v)) []]
    toContents v@SubBlock =
	[mkElemC (showConstr 4 (toHType v)) []]
    toContents v@SubPointy =
	[mkElemC (showConstr 5 (toHType v)) []]
    toContents v@SubPrim =
	[mkElemC (showConstr 6 (toHType v)) []]

instance Perl5 Val where
    showPerl5 (VUndef) = showP5Class "VUndef"
    showPerl5 (VBool aa) = showP5ArrayObj "VBool" [showPerl5 aa]
    showPerl5 (VInt aa) = showP5ArrayObj "VInt" [showPerl5 aa]
    showPerl5 (VRat aa) = showP5ArrayObj "VRat" [showPerl5 aa]
    showPerl5 (VNum aa) = showP5ArrayObj "VNum" [showPerl5 aa]
    showPerl5 (VStr aa) = showP5ArrayObj "VStr" [showPerl5 aa]
    showPerl5 (VList aa) = showP5ArrayObj "VList" [showPerl5 aa]
    showPerl5 (VType aa) = showP5ArrayObj "VType" [showPerl5 aa]

instance Haskell2Xml Val where
    toHType v =
	Defined "Val" []
		[Constr "VUndef" [] [],Constr "VBool" [] [toHType aa],
		 Constr "VInt" [] [toHType ab],Constr "VRat" [] [toHType ac],
		 Constr "VNum" [] [toHType ad],Constr "VStr" [] [toHType ae],
		 Constr "VList" [] [toHType af],Constr "VType" [] [toHType ag]]
      where
	(VBool aa) = v
	(VInt ab) = v
	(VRat ac) = v
	(VNum ad) = v
	(VStr ae) = v
	(VList af) = v
	(VType ag) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "VUndef" `isPrefixOf` constr =
	    (VUndef,etc)
	| "VType" `isPrefixOf` constr =
	    (\(ag,_)-> (VType ag, etc)) (fromContents cs)
	| "VStr" `isPrefixOf` constr =
	    (\(ae,_)-> (VStr ae, etc)) (fromContents cs)
	| "VRat" `isPrefixOf` constr =
	    (\(ac,_)-> (VRat ac, etc)) (fromContents cs)
	| "VNum" `isPrefixOf` constr =
	    (\(ad,_)-> (VNum ad, etc)) (fromContents cs)
	| "VList" `isPrefixOf` constr =
	    (\(af,_)-> (VList af, etc)) (fromContents cs)
	| "VInt" `isPrefixOf` constr =
	    (\(ab,_)-> (VInt ab, etc)) (fromContents cs)
	| "VBool" `isPrefixOf` constr =
	    (\(aa,_)-> (VBool aa, etc)) (fromContents cs)
    toContents v@VUndef =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@(VBool aa) =
	[mkElemC (showConstr 1 (toHType v)) (toContents aa)]
    toContents v@(VInt ab) =
	[mkElemC (showConstr 2 (toHType v)) (toContents ab)]
    toContents v@(VRat ac) =
	[mkElemC (showConstr 3 (toHType v)) (toContents ac)]
    toContents v@(VNum ad) =
	[mkElemC (showConstr 4 (toHType v)) (toContents ad)]
    toContents v@(VStr ae) =
	[mkElemC (showConstr 5 (toHType v)) (toContents ae)]
    toContents v@(VList af) =
	[mkElemC (showConstr 6 (toHType v)) (toContents af)]
    toContents v@(VType ag) =
	[mkElemC (showConstr 7 (toHType v)) (toContents ag)]

instance Perl5 Cxt where
    showPerl5 (CxtVoid) = showP5Class "CxtVoid"
    showPerl5 (CxtItem aa) = showP5ArrayObj "CxtItem" [showPerl5 aa]
    showPerl5 (CxtSlurpy aa) = showP5ArrayObj "CxtSlurpy"
	      [showPerl5 aa]

instance Haskell2Xml Cxt where
    toHType v =
	Defined "Cxt" []
		[Constr "CxtVoid" [] [],Constr "CxtItem" [] [toHType aa],
		 Constr "CxtSlurpy" [] [toHType ab]]
      where
	(CxtItem aa) = v
	(CxtSlurpy ab) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "CxtVoid" `isPrefixOf` constr =
	    (CxtVoid,etc)
	| "CxtSlurpy" `isPrefixOf` constr =
	    (\(ab,_)-> (CxtSlurpy ab, etc)) (fromContents cs)
	| "CxtItem" `isPrefixOf` constr =
	    (\(aa,_)-> (CxtItem aa, etc)) (fromContents cs)
    toContents v@CxtVoid =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@(CxtItem aa) =
	[mkElemC (showConstr 1 (toHType v)) (toContents aa)]
    toContents v@(CxtSlurpy ab) =
	[mkElemC (showConstr 2 (toHType v)) (toContents ab)]

instance Perl5 Type where
    showPerl5 (MkType aa) = showP5ArrayObj "MkType" [showPerl5 aa]
    showPerl5 (TypeOr aa ab) = showP5ArrayObj "TypeOr"
	      [showPerl5 aa , showPerl5 ab]
    showPerl5 (TypeAnd aa ab) = showP5ArrayObj "TypeAnd"
	      [showPerl5 aa , showPerl5 ab]

instance Haskell2Xml Type where
    toHType v =
	Defined "Type" []
		[Constr "MkType" [] [toHType aa],
		 Constr "TypeOr" [] [toHType ab,toHType ac],
		 Constr "TypeAnd" [] [toHType ad,toHType ae]]
      where
	(MkType aa) = v
	(TypeOr ab ac) = v
	(TypeAnd ad ae) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "TypeOr" `isPrefixOf` constr =
	    (\(ab,cs00)-> (\(ac,_)-> (TypeOr ab ac, etc)) (fromContents cs00))
	    (fromContents cs)
	| "TypeAnd" `isPrefixOf` constr =
	    (\(ad,cs00)-> (\(ae,_)-> (TypeAnd ad ae, etc)) (fromContents cs00))
	    (fromContents cs)
	| "MkType" `isPrefixOf` constr =
	    (\(aa,_)-> (MkType aa, etc)) (fromContents cs)
    toContents v@(MkType aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(TypeOr ab ac) =
	[mkElemC (showConstr 1 (toHType v)) (concat [toContents ab,
						     toContents ac])]
    toContents v@(TypeAnd ad ae) =
	[mkElemC (showConstr 2 (toHType v)) (concat [toContents ad,
						     toContents ae])]

instance Perl5 Param where
    showPerl5 (MkParam aa ab ac ad ae af ag ah ai) =
	      showP5HashObj "MkParam"
	      [("isInvocant", showPerl5 aa) , ("isOptional", showPerl5 ab) ,
	       ("isNamed", showPerl5 ac) , ("isLValue", showPerl5 ad) ,
	       ("isWritable", showPerl5 ae) , ("isLazy", showPerl5 af) ,
	       ("paramName", showPerl5 ag) , ("paramContext", showPerl5 ah) ,
	       ("paramDefault", showPerl5 ai)]

instance Haskell2Xml Param where
    toHType v =
	Defined "Param" []
		[Constr "MkParam" []
			[toHType aa,toHType ab,toHType ac,toHType ad,toHType ae,toHType af,
			 toHType ag,toHType ah,toHType ai]]
      where
	(MkParam aa ab ac ad ae af ag ah ai) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "MkParam" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,cs02)-> (\(ad,cs03)-> (\(ae,
								       cs04)-> (\(af,cs05)-> (\(ag,
												cs06)-> (\(ah
													   ,
													   cs07)-> (\(ai
														      ,
														      _)-> (MkParam aa ab ac ad ae af ag ah ai, etc))
														   (fromContents cs07))
													(fromContents cs06))
											     (fromContents cs05))
									       (fromContents cs04))
								    (fromContents cs03))
						      (fromContents cs02))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(MkParam aa ab ac ad ae af ag ah ai) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab,toContents ac,toContents ad,
						     toContents ae,toContents af,toContents ag,
						     toContents ah,toContents ai])]

instance Perl5 Pos where
    showPerl5 (MkPos aa ab ac ad ae) = showP5HashObj "MkPos"
	      [("posName", showPerl5 aa) , ("posBeginLine", showPerl5 ab) ,
	       ("posBeginColumn", showPerl5 ac) , ("posEndLine", showPerl5 ad) ,
	       ("posEndColumn", showPerl5 ae)]

instance Haskell2Xml Pos where
    toHType v =
	Defined "Pos" []
		[Constr "MkPos" []
			[toHType aa,toHType ab,toHType ac,toHType ad,toHType ae]]
      where
	(MkPos aa ab ac ad ae) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "MkPos" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,cs02)-> (\(ad,cs03)-> (\(ae,
								       _)-> (MkPos aa ab ac ad ae, etc))
								    (fromContents cs03))
						      (fromContents cs02))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(MkPos aa ab ac ad ae) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab,toContents ac,toContents ad,
						     toContents ae])]

--  Imported from other files :-
