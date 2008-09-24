module M0ld.AST where
type Register = [Char]
type Label = [Char]
data Value = Var [Char] | IntegerConstant Integer | StringConstant [Char] | None | SubMold [Stmt]
    deriving (Show,Eq,Ord)
data Capture = Capture Register [Register] [Register]
    deriving (Show,Eq,Ord)
data Stmt = LabelDef Label | Decl Register Value | Goto Label | Br Register Label Label | Call Register Register Capture | Call2 Register Register Register Register  
    deriving (Show,Eq,Ord)
data Argument = Pos Register | Named Register Register

data Mold = Mold [Stmt]
    deriving Show
