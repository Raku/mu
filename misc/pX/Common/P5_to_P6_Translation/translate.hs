import Text.ParserCombinators.Parsec
import IO hiding (try)
{------------------
The P5AST structure represents the abstract syntax tree
of a perl 5 program. It is made up of two types of nodes:
AbstractNodes (!perl/P5AST in the yaml files) just have a nodetype and kids (0+)
LiteralNodes (!perl/p5 in the yaml files) have two strings, for the enc and uni fields.
-------------------}

data P5AST
    = AbstractNode AbsType [P5AST]
    | LiteralNode LitType String String
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
    | Op_chdir
    | Op_const
    | Op_entersub
    | Op_ftdir
    | Op_helem
    | Op_leave
    | Op_lineseq
    | Op_list
    | Op_match
    | Op_method
    | Op_null
    | Op_require
    | Op_rv2av
    | Op_rv2hv
    | Op_rv2sv
    | Op_sassign
    | Op_subst
    | Package
    | Peg
    | Statement
    | Sub
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
    | Sigil
    | Text
    | Token
    | UnknownLit
    deriving (Show, Eq, Read) 

{-------------
nodeNamer is parsec parser that parses nodes,
recursivley parsing child nodes. It has two distinct cases,
one for nodes with kids, one for all other nodes.
---------------}
nodeNamer :: Int -> Parser P5AST
nodeNamer indent = do
    count indent space
    withKids indent <|> noKids indent

withKids :: Int -> Parser P5AST
withKids indent = do
    try (string "- !perl/P5AST::") <?> "P5AST decleration";
    name <- manyTill anyChar space
    newline
    spaces
    string "Kids: " <?> "Kids"
    modifier <- manyTill anyChar newline
    kids <- case modifier of
        "[]"    -> (newline <?> "newline") >> return []
        _       -> many . try $ nodeNamer (indent+4)
    let con = case name of
            "condmod"       -> Condmod
            "listelem"      -> Listelem
            "nothing"       -> PNothing
            "op_aassign"    -> Op_aassign
            "op_chdir"      -> Op_chdir
            "op_const"      -> Op_const
            "op_ftdir"      -> Op_ftdir
            "op_helem"      -> Op_helem
            "op_leave"      -> Op_leave
            "op_list"       -> Op_list
            "op_null"       -> Op_null
            "op_rv2av"      -> Op_rv2av
            "op_rv2hv"      -> Op_rv2hv
            "op_rv2sv"      -> Op_rv2sv
            "op_sassign"    -> Op_sassign
            "op_subst"      -> Op_subst
            "package"       -> Package
            "peg"           -> Peg
            "statement"     -> Statement
            _               -> UnknownAbs
    return $AbstractNode con kids

noKids :: Int -> Parser P5AST
noKids indent = do
    string "- !perl/p5::" <?> "p5 decleration"
    name <- manyTill anyChar space
    manyTill anyToken newline
    spaces
    string "enc: "
    enc <- manyTill anyChar newline <?> "enc string"
    spaces
    string "uni: "
    --Uniblock deals with the various types of yaml blocks
    uni <- uniBlock (indent + 4) <?> "uni string/block"
    let con = case name of
            "closer"        -> Closer
            "closequote"    -> Closequote
            "junk"          -> Junk
            "opener"        -> Opener
            "openquote"     -> Openquote
            "operator"      -> Operator
            "punct"         -> Punct
            "sigil"         -> Sigil
            "text"          -> Text
            "token"         -> Token
            _               -> UnknownLit 
    return $ LiteralNode con enc uni

{-
Uniblock handles the various types of yaml blocks used, those being a literal string (i.e. "...")
A block "|\n ..." or a block with a chomp modifier "|+\n ..."
-}
uniBlock :: Int -> Parser String
uniBlock indent = choice
    [do try $ string "|\n"
        uni <- manyTill (manyTill anyToken newline) (try(newline)) <?> "uni block";
        return (unlines (map (drop indent) uni))
    ,do try $ string "|+"
        newline;
        uni <- manyTill (manyTill anyToken newline) (try(newline)) <?> "uni block with chomp modifier";
        return (unlines (map (drop indent) uni))
    ,do uni <- manyTill anyToken newline <?> "uni string";
        --If the field is in quotes, strip the quotes by stripping the first character, 
        --reversing the string, stripping the first character, then reversing again
        return $ if (head uni `elem` "\"'")
            then reverse (tail (reverse (tail uni)))
            else uni
    ]

--A wrapper for nodeNamer, to handle the junk at the beginning of the file.
parseInput :: Parser [P5AST]
parseInput = do
    sequence_ (replicate 3 $ manyTill anyToken newline)
    names <- many (nodeNamer 2)
    eof 
    return names

{- No longer a big big messy function to print all the different node types, 
now a slim function to print everything to a file.
Only two cases: LiteralNode and AbstractNode.
For a literal node, print the uni field.
For an abstract node, recursivley call printTree on the kids (if there are any).
All output is to a file
-}
printTree :: Handle -> P5AST -> IO ()
printTree outFile (LiteralNode _ _ uni) = hPutStr outFile uni
printTree outFile (AbstractNode _ []) = hPutStr outFile ""
printTree outFile (AbstractNode _ kids) = do{ printTree outFile (head kids);
                                              printTree outFile (AbstractNode P5AST (tail kids))}

--Wrapper function to apply all translations in order
translate :: P5AST -> P5AST
translate tree = ((equalTildeToTildeTilde tree))

{-Translates =~ -> ~~ for using regexs in P6
The name of the function is a bit long, but it won't be called often
and at least it's very descriptive -}
equalTildeToTildeTilde :: P5AST -> P5AST
equalTildeToTildeTilde (LiteralNode Operator enc "=~") = (LiteralNode Operator enc "~~")
equalTildeToTildeTilde (AbstractNode atype kids) = (AbstractNode atype (map equalTildeToTildeTilde  kids))
equalTildeToTildeTilde (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-
A main function to parse a file containing a tree and output the contents to another file
Useage: mainParse inFile outFile
-}
mainParse :: FilePath -> FilePath -> IO ()
mainParse inName outName = do
    inHandle    <- openFile inName ReadMode
    input       <- hGetContents inHandle
    outHandle   <- openFile outName WriteMode
    -- putStrLn ("DEBUG: got input " ++ input)
    let dirs = case parse parseInput "stdin" input of
            Left err -> error $ "Input:\n" ++ show input ++ 
                                "\nError:\n" ++ show err
            Right result -> result
    putStrLn "DEBUG: parsed:";
    print (AbstractNode P5AST dirs)
    hClose inHandle
    printTree outHandle (translate (AbstractNode P5AST dirs))
    hClose outHandle
    putStrLn "Finished"
