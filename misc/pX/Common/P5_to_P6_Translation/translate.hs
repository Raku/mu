import Text.ParserCombinators.Parsec
import IO
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
nodeNamer:: Int -> Parser P5AST
nodeNamer indent = do{count indent space;
                     do{
                        Text.ParserCombinators.Parsec.try(string "- !perl/P5AST::") <?> "P5AST decleration";
                        name <- manyTill anyChar space;
                        newline;
                        spaces;
                        string "Kids: " <?> "Kids";
                        modifier <- manyTill anyChar newline;
                        kids <- if (modifier == "[]") then do{newline <?> "newline"; return []} else
                                 do{kids <- many (Text.ParserCombinators.Parsec.try(nodeNamer (indent+4))); return kids};
                        --This block is very ugly, but for some reason I can't get it to work any other way. Changes welcome. -Sage
                        if (name == "condmod") then return (Condmod kids) else
                         if (name == "listelem") then return (Listelem kids) else 
                          if (name == "nothing") then return (PNothing kids) else 
                           if(name == "op_aassign") then return (Op_aassign kids) else
                            if(name == "op_chdir") then return (Op_chdir kids) else  
                             if(name == "op_const") then return (Op_const kids) else
                              if(name == "op_ftdir") then return (Op_ftdir kids) else
                               if(name == "op_helem") then return (Op_helem kids) else
                                if(name == "op_leave") then return (Op_leave kids) else
                                 if(name == "op_list") then return (Op_list kids) else
                                  if(name == "op_null") then return (Op_null kids) else
                                   if(name == "op_rv2av") then return (Op_rv2av kids) else
                                    if(name == "op_rv2hv") then return (Op_rv2hv kids) else
                                     if(name == "op_rv2sv") then return (Op_rv2sv kids) else
                                      if(name == "op_sassign") then return (Op_sassign kids) else
                                       if(name == "op_subst") then return (Op_subst kids) else
                                        if(name == "package") then return (Package kids) else
                                         if(name == "peg") then return (Peg kids) else
                                          if(name == "statement") then return (Statement kids) else
                                           return (Unknown "1" "AST")
                       }<|>do{ 
                        string "- !perl/p5::" <?> "p5 decleration";
                        name <- manyTill anyChar space;
                        manyTill anyToken newline;
                        spaces;
                        string "enc: ";
                        enc <- manyTill anyChar newline <?> "enc string";
                        spaces;
                        string "uni: ";
                        uni <- (uniBlock (indent +4) <?> "uni string/block"); --Uniblock deals with the various types of yaml blocks
                        --This block is very ugly, but for some reason I can't get it to work any other way. Changes welcome. -Sage
                        if(name == "closer") then return (Closer enc uni) else
                         if(name == "closequote") then return (Closequote enc uni) else
                          if(name == "junk") then return (Junk enc uni) else
                           if(name == "opener") then return (Opener enc uni) else 
                            if(name == "openquote") then return (Openquote enc uni) else 
                             if(name == "operator") then return (Operator enc uni) else
                              if(name == "punct") then return (Punct enc uni) else
                               if(name == "sigil") then return (Sigil enc uni) else
                                if(name == "text") then return (Text enc uni) else
                                 if(name == "token") then return (Token enc uni) else
                                  return (Unknown enc uni)
                        }
                    }

{-
Uniblock handles the various types of yaml blocks used, those being a literal string (i.e. "...")
A block "|\n ..." or a block with a chomp modifier "|+\n ..."
-}
uniBlock :: Int -> Parser String
uniBlock indent = do{ Text.ParserCombinators.Parsec.try(string "|\n"); 
                      uni <- manyTill (manyTill anyToken newline) (Text.ParserCombinators.Parsec.try(newline)) <?> "uni block";
                      return (unlines (map (drop indent) uni))
                    }<|>
                  do{ Text.ParserCombinators.Parsec.try(string "|+");
                      newline;
                      uni <- manyTill (manyTill anyToken newline) (Text.ParserCombinators.Parsec.try(newline)) <?> "uni block with chomp modifier";
                      return (unlines (map (drop indent) uni))
                    }<|>
                  do{ uni <- manyTill anyToken newline <?> "uni string";
                      --If the field is in quotes, strip the quotes by stripping the first character, 
                      --reversing the string, stripping the first character, then reversing again
                      if (or [((head uni)=='"'), ((head uni)=='\'')]) then return (reverse (tail (reverse (tail uni)))) else
                       return uni
                    }

--A wrapper for nodeNamer, to handle the junk at the beginning of the file.
parseInput :: Parser [P5AST]
parseInput = do manyTill anyToken newline;
                manyTill anyToken newline;
                manyTill anyToken newline;
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
mainParse inName outName= do inHandle <- openFile inName ReadMode
                             input <- hGetContents inHandle
                             outHandle <- openFile outName WriteMode
                             -- putStrLn ("DEBUG: got input " ++ input)
                             let dirs = case parse parseInput "stdin" input of
                                             Left err -> error $ "Input:\n" ++ show input ++ 
                                                                 "\nError:\n" ++ show err
                                             Right result -> result
                             putStrLn "DEBUG: parsed:"; 
                             print (P5AST dirs);
                             hClose inHandle;
                             printTree outHandle (P5AST dirs);
                             hClose outHandle;
                             putStrLn "Finished"