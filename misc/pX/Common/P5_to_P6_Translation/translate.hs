import Text.ParserCombinators.Parsec
import IO hiding (try)
{------------------
The P5AST structure represents the abstract syntax tree
of a perl 5 program. Nodes with kids store kids in a list,
otherwise a node holds two strings.
-------------------}
data P5AST
    = P5AST [P5AST]
    | Closer String String 
    | Closequote String String 
    | Condmod [P5AST]
    | Junk String String
    | Listelem [P5AST]
    | PNothing [P5AST]
    | Op_aassign [P5AST]
    | Op_chdir [P5AST]
    | Op_const [P5AST]
    | Op_ftdir [P5AST]
    | Op_helem [P5AST]
    | Op_leave [P5AST]
    | Op_list [P5AST]
    | Op_null [P5AST]
    | Op_rv2av [P5AST]
    | Op_rv2hv [P5AST]
    | Op_rv2sv [P5AST]
    | Op_sassign [P5AST]
    | Op_subst [P5AST]
    | Opener String String
    | Openquote String String
    | Operator String String
    | Package[P5AST]
    | Peg [P5AST]
    | Punct String String
    | Sigil String String
    | Statement [P5AST]
    | Text String String 
    | Token String String 
    | Unknown String String
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
            _               -> const (Unknown "1" "AST")
    return $ con kids

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
            _               -> Unknown 
    return $ con enc uni

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

{- A big big messy function to print all the different node types
There has to be a case to everything, unfortunately, so this function is very large
when the work is very simple: if the node is a leaf, print the "uni" part of it.
if the node has kids, just recursively call printTree on all kids.
All output is to a file
-}
printTree :: Handle -> P5AST -> IO ()
printTree outFile (Closer _ uni) = hPutStr outFile uni
printTree outFile (Closequote _ uni) = hPutStr outFile uni
printTree outFile (Junk _ uni) = hPutStr outFile uni
printTree outFile (Opener _ uni) = hPutStr outFile uni
printTree outFile (Openquote _ uni) = hPutStr outFile uni
printTree outFile (Operator _ uni) = hPutStr outFile uni
printTree outFile (Punct _ uni) = hPutStr outFile uni
printTree outFile (Sigil _ uni) = hPutStr outFile uni
printTree outFile (Text _ uni) = hPutStr outFile uni
printTree outFile (Token _ uni) = hPutStr outFile uni
printTree outFile (Unknown _ uni) = hPutStr outFile uni
printTree outFile (P5AST []) = hPutStr outFile ""
printTree outFile (P5AST kids) = do{ printTree outFile (head kids);
                                       printTree outFile (P5AST (tail kids))}
printTree outFile (Condmod []) = hPutStr  outFile ""
printTree outFile (Condmod kids) = do{ printTree outFile (head kids);
                                       printTree outFile (Condmod (tail kids))}
printTree outFile (Listelem []) = hPutStr outFile ""
printTree outFile (Listelem kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Listelem (tail kids))}
printTree outFile (PNothing []) = hPutStr outFile ""
printTree outFile (PNothing kids) = do{ printTree outFile (head kids);
                                        printTree outFile (PNothing (tail kids))}
printTree outFile (Op_aassign []) = hPutStr outFile ""
printTree outFile (Op_aassign kids) = do{ printTree outFile (head kids);
                                          printTree outFile (Op_aassign (tail kids))}
printTree outFile (Op_chdir []) = hPutStr outFile ""
printTree outFile (Op_chdir kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_chdir (tail kids))}
printTree outFile (Op_const []) = hPutStr outFile ""
printTree outFile (Op_const kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_const (tail kids))}
printTree outFile (Op_ftdir []) = hPutStr outFile ""
printTree outFile (Op_ftdir kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_ftdir (tail kids))}
printTree outFile (Op_helem []) = hPutStr outFile ""
printTree outFile (Op_helem kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_helem (tail kids))}
printTree outFile (Op_leave []) = hPutStr outFile ""
printTree outFile (Op_leave kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_leave (tail kids))}
printTree outFile (Op_list []) = hPutStr outFile ""
printTree outFile (Op_list kids) = do{ printTree outFile (head kids);
                                       printTree outFile (Op_list (tail kids))}
printTree outFile (Op_null []) = hPutStr outFile ""
printTree outFile (Op_null kids) = do{ printTree outFile (head kids);
                                       printTree outFile (Op_null (tail kids))}
printTree outFile (Op_rv2av []) = hPutStr outFile ""
printTree outFile (Op_rv2av kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_rv2av (tail kids))}
printTree outFile (Op_rv2hv []) = hPutStr outFile ""
printTree outFile (Op_rv2hv kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_rv2hv (tail kids))}
printTree outFile (Op_rv2sv []) = hPutStr outFile ""
printTree outFile (Op_rv2sv kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_rv2sv (tail kids))}
printTree outFile (Op_sassign []) = hPutStr outFile ""
printTree outFile (Op_sassign kids) = do{ printTree outFile (head kids);
                                          printTree outFile (Op_sassign (tail kids))}
printTree outFile (Op_subst []) = hPutStr outFile ""
printTree outFile (Op_subst kids) = do{ printTree outFile (head kids);
                                        printTree outFile (Op_subst (tail kids))}
printTree outFile (Package []) = hPutStr outFile ""
printTree outFile (Package kids) = do{ printTree outFile (head kids);
                                       printTree outFile (Package (tail kids))}
printTree outFile (Peg []) = hPutStr outFile ""
printTree outFile (Peg kids) = do{ printTree outFile (head kids);
                                   printTree outFile (Peg (tail kids))}
printTree outFile (Statement []) = hPutStr outFile ""
printTree outFile (Statement kids) = do{ printTree outFile (head kids);
                                         printTree outFile (Statement (tail kids))}

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
    print (P5AST dirs)
    hClose inHandle
    printTree outHandle (P5AST dirs)
    hClose outHandle
    putStrLn "Finished"
