module ASTParser where
{-----------------------
"Words. Nothing but sweet, sweet words that turn into bitter orange wax in my ears."
-Fry, Futurama

This module parses yaml files created by Larry Wall's MADSKILLS parser.
It is designed to be used with ASTTranslate, but feel free to use it elsewhere
if it's useful.

Parsec is used for parsing, which should come standard with GHC.

See ASTTranslate.hs for more.

Originally created by Sage LaTorra for Summer of Code 2006.
------------------------}
import Text.ParserCombinators.Parsec
import IO hiding (try)
import ASTUtil
import ASTDefinition

{-------------
nodeNamer is parsec parser that parses nodes, recursivley parsing child nodes. It has two distinct cases, 
one for nodes with kids, one for all other nodes.
---------------}
nodeNamer :: Int -> Parser P5AST
nodeNamer indent = do
    choice[(count indent space),(count 0 space)] --The count 0 space option accounts for nodes after the chomp modifier on a uni block, since the uni block will consume everything up to the '-'
    choice [hereDoc indent, withKids indent , noKids indent , blank indent]


{-A blank looks like 
- ''
and since it needs to return _something_, it just returns junk with no real text.
The int parameter is the whitespace it expects.
-}
blank :: Int -> Parser P5AST
blank indent = do
    try (string "- ''")
    newline
    return (LiteralNode "junk" "1" "")

{-
A node with kids has a nodetype and a list of nodes (which may be empty).
The int parameter is the whitespace it expects.
-}
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
    return $AbstractNode name kids


{-
A node with kids has a type, and enc and uni fields.
The int parameter is the whitespace it expects.
-}
noKids :: Int -> Parser P5AST
noKids indent = do
    try (string "- !perl/p5::") <?> "p5 decleration"
    name <- manyTill anyChar space
    manyTill anyToken newline
    spaces
    string "enc: "
    enc <- manyTill anyChar newline <?> "enc string"
    spaces
    string "uni: "
    --Uniblock deals with the various types of yaml blocks
    uni <- uniBlock (indent + 4) <?> "uni string/block"
    return $ LiteralNode name enc uni

{-
A heredoc is basically a node with kids, but it also has a start and an end node.
The int parameter is the whitespace it expects.
-}
hereDoc :: Int -> Parser P5AST
hereDoc indent = do
    try (string "- !perl/P5AST::heredoc ") <?> "Heredoc decleration";
    newline
    spaces
    string "doc: !perl/P5AST::"
    doc <- manyTill anyChar space
    newline
    spaces
    string "Kids: "
    newline
    kids <- many (try (nodeNamer (indent+6)))
    spaces
    string "end: !perl/p5::closequote "
    newline
    spaces
    string "enc: "
    endenc <- manyTill anyChar newline
    spaces
    string "uni: "
    enduni <- uniBlock (indent + 6) 
    spaces
    string "start: !perl/p5::openquote "
    newline
    spaces
    string "enc: "
    startenc <- manyTill anyChar newline
    spaces
    string "uni: "
    startuni <- uniBlock (indent + 6)
    return (Heredoc doc (LiteralNode "openquote" startenc startuni) (LiteralNode "closequote" endenc enduni) kids)
    
    
    

    
{-
Uniblock handles the various types of yaml blocks used, those being a literal string (i.e. "..." or even just ...)
A block "|\n ..." or a block with a chomp modifier "|+\n ..."
-}
uniBlock :: Int -> Parser String
uniBlock indent = choice
    [do try $ string "|\n"
        uni <- manyTill (manyTill anyToken newline) (try(newline)) <?> "uni block";
        return (unlines (map (drop indent) uni))
    ,do try $ string "|+"
        newline;
        uni <- manyTill (manyTill anyToken newline) (try(choice [try(do{count (indent-4) space; notFollowedBy (char ' ')}), try(do{count (indent-8) space; notFollowedBy (char ' ')}), try(eof)])) <?> "uni block with chomp modifier"; -- A block with the chomp modifier ends when there's a line with too few spaces (which means another node (or the end part of a heredoc) is starting.
        return (unlines (map (drop indent) uni))
    ,do try $ string "|-"
        newline;
        uni <- manyTill (manyTill anyToken newline) (try(choice [try(do{count (indent-4) space; notFollowedBy (char ' ')}), try(do{count (indent-8) space; notFollowedBy (char ' ')})])) <?> "uni block with chomp modifier"; -- A block with the chomp modifier ends when there's a line with too few spaces (which means another node (or the end part of a heredoc) is starting.
        return (unlines (map (drop indent) uni))
    ,do try $ string "\""
        uni <- manyTill anyToken (try(string("\"\n")))
        --return uni
        return $ if ((length (lines uni))==1) then (makeLiterals uni) else (makeLiterals (joinString ((head (lines (getRidOfExtraSlashes uni))):(map (drop indent) (tail (lines (getRidOfExtraSlashes uni)))))))
    ,do uni <- manyTill anyToken newline <?> "uni string"
        --If the field is in quotes, strip the quotes by stripping the first character, 
        --reversing the string, stripping the first character, then reversing again
        return $ if (head uni `elem` "\"'")
            then (makeLiterals (reverse (tail (reverse (tail uni)))))
            else uni
    ]


--A wrapper for nodeNamer, to handle the junk at the beginning of the file.

parseInput :: Parser [P5AST]
parseInput = choice [do{try(manyTill anyToken (string "Kids: \n")); names <- many (nodeNamer 2); eof; return names}, return []]


{-This function deals with extra slashes that show up in text -}

getRidOfExtraSlashes :: String -> String
getRidOfExtraSlashes []   = []
getRidOfExtraSlashes inSt = if (and [((head inSt)=='\\'), ((head (tail inSt))=='\n')]) then ('\n':(getRidOfExtraSlashes (drop 2 inSt))) else ((head inSt):(getRidOfExtraSlashes (tail inSt)))


{-Function to handle escaped characters in a string scanned from input.
For example, if the string "blah\n" is scanned, it ends up being represented as
"blah\\n". This function parses that newline into a literal newline.-}

makeLiterals :: String -> String
makeLiterals []   = []
makeLiterals inSt = if ((head inSt)=='\\') then if (head (tail inSt) == '"') then ('\"':(makeLiterals(drop 2 inSt))) else
                                                   if (head (tail inSt) == 'n') then ('\n':(makeLiterals(drop 2 inSt))) else
                                                     if (head (tail inSt) == 't') then ('\t':(makeLiterals(drop 2 inSt))) else
                                                       if(head (tail inSt) == '\n') then ('\n':(makeLiterals(drop 2 inSt))) else
                                                         if (head (tail inSt) == '\\') then ('\\':(makeLiterals(drop 2 inSt))) else 
                                                           ((makeLiterals(tail inSt)))
                      else ((head inSt):(makeLiterals (tail inSt)))


{- No longer a big big messy function to print all the different node types, 
now a slim function to print everything to a file.
Only two cases: LiteralNode and AbstractNode.
For a literal node, print the uni field.
For an abstract node, recursivley call printTree on the kids (if there are any).
All output is to a file
-}

printTree :: Handle -> P5AST -> String -> IO ()
printTree outFile (LiteralNode atype _ uni) options    = if (and [(atype=="UnknownLit"),('u' `elem` options)]) then do{ hPutStr outFile "UnknownLit"; putStrLn "UNKNOWN: UnknownLit"; hPutStr outFile uni} else (hPutStr outFile uni)
printTree outFile (AbstractNode atype []) options      = if (and [(atype=="UnknownAbs"),('u' `elem` options)]) then do{ hPutStr outFile "UnknownAbs"; putStrLn "UNKNOWN: UnknownAbs"; hPutStr outFile ""} else (hPutStr outFile "")
printTree outFile (AbstractNode atype kids) options    = if (and [(atype=="UnknownAbs"),('u' `elem` options)]) then do{ hPutStr outFile "UnknownAbs"; putStrLn "UNKNOWN: UnknownAbs"; printTree outFile (head kids) options; printTree outFile (AbstractNode "P5AST" (tail kids)) options} else do{ printTree outFile (head kids) options; printTree outFile (AbstractNode "P5AST" (tail kids)) options}
printTree outFile (Heredoc doc start end kids) options = do printTree outFile start options
    hPutStr outFile ";\n"
    printTree outFile (AbstractNode "P5AST" kids) options
    printTree outFile end options

