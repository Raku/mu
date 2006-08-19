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
    = AbstractNode String [P5AST]
    | LiteralNode String String String
    | Heredoc String P5AST P5AST  [P5AST] -- That is, Heredoc Start End Doc
    deriving (Show, Eq, Read)

