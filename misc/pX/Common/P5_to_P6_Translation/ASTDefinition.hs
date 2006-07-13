module ASTDefinition where
{------------------------
This module defines the AST as used by ASTParser and ASTTranslate (and ASTUtil also, for that matter).
It's really a very simple structure, as described bellow. It is designed to be used with ASTTranslate, 
but feel free to use it elsewhere if it's useful.

See ASTTranslate.hs for more.

Originally created by Sage LaTorra for Summer of Code 2006.
-------------------------}


{------------------
The P5AST structure represents the abstract syntax tree
of a perl 5 program. It is made up of two types of nodes:
AbstractNodes (!perl/P5AST in the yaml files) just have a nodetype and kids (0+)
LiteralNodes (!perl/p5 in the yaml files) have two strings, for the enc and uni fields.
-------------------}

data P5AST
    = AbstractNode AbsType [P5AST]
    | LiteralNode LitType String String
    | Heredoc P5AST P5AST  [P5AST] -- That is, Heredoc Start End Doc
    deriving (Show, Eq, Read)

{----------------
These are the types for AbstractNodes. This list may not be exhaustive,
but it covers enough basic cases to handle the tree of TestInit.pm
-----------------}
data AbsType
    = P5AST
    | Condmod
    | Condstate
    | Listelem
    | PNothing
    | Op_aassign
    | Op_aelem
    | Op_chdir
    | Op_close
    | Op_const
    | Op_cond_expr
    | Op_entersub
    | Op_ftdir
    | Op_gv
    | Op_helem
    | Op_iter
    | Op_leave
    | Op_leaveloop
    | Op_length
    | Op_lineseq
    | Op_list
    | Op_match
    | Op_method
    | Op_not
    | Op_null
    | Op_padav
    | Op_padsv
    | Op_print
    | Op_pushmark
    | Op_readline
    | Op_require
    | Op_rv2av
    | Op_rv2gv
    | Op_rv2hv
    | Op_rv2sv
    | Op_sassign
    | Op_split
    | Op_subst
    | Op_stringify
    | Package
    | Peg
    | Statement
    | Sub
    | Ternary
    | UnknownAbs
    deriving (Show, Eq, Read)

{----------------
These are the types for LiteralNodes. This list may not be exhaustive,
but it covers enough basic cases to handle the tree of TestInit.pm
-----------------}
data LitType
    = Closer
    | Closequote
    | Declarator
    | Junk
    | Opener
    | Openquote
    | Operator
    | Punct
    | Remod
    | Sigil
    | Text
    | Token
    | UnknownLit
    deriving (Show, Eq, Read)