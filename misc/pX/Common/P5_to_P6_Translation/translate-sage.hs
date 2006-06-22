import Text.ParserCombinators.Parsec
import IO
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
                        if (name == "condmod") then return (AbstractNode Condmod kids) else
                         if (name == "condstate") then return (AbstractNode Condstate kids) else
                          if (name == "listelem") then return (AbstractNode Listelem kids) else 
                           if (name == "nothing") then return (AbstractNode PNothing kids) else 
                            if(name == "op_aassign") then return (AbstractNode Op_aassign kids) else
                             if(name == "op_chdir") then return (AbstractNode Op_chdir kids) else  
                              if(name == "op_const") then return (AbstractNode Op_const kids) else
                               if (name == "op_entersub") then return (AbstractNode Op_entersub kids) else
                                if(name == "op_ftdir") then return (AbstractNode Op_ftdir kids) else
                                 if(name == "op_helem") then return (AbstractNode Op_helem kids) else
                                  if(name == "op_leave") then return (AbstractNode Op_leave kids) else
                                   if (name == "op_lineseq") then return (AbstractNode Op_lineseq kids) else
                                    if(name == "op_list") then return (AbstractNode Op_list kids) else
                                     if (name == "op_match") then return (AbstractNode Op_match kids) else
                                      if (name == "op_method") then return (AbstractNode Op_method kids) else
                                       if(name == "op_null") then return (AbstractNode Op_null kids) else
                                        if(name == "op_rv2av") then return (AbstractNode Op_rv2av kids) else
                                         if(name == "op_rv2hv") then return (AbstractNode Op_rv2hv kids) else
                                          if(name == "op_rv2sv") then return (AbstractNode Op_rv2sv kids) else
                                           if(name == "op_sassign") then return (AbstractNode Op_sassign kids) else
                                            if(name == "op_subst") then return (AbstractNode Op_subst kids) else
                                             if(name == "package") then return (AbstractNode Package kids) else
                                              if(name == "peg") then return (AbstractNode Peg kids) else
                                               if(name == "statement") then return (AbstractNode Statement kids) else
                                                if (name == "sub") then return (AbstractNode Sub kids) else
                                                 return (AbstractNode UnknownAbs kids)
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
                        if(name == "closer") then return (LiteralNode Closer enc uni) else
                         if(name == "closequote") then return (LiteralNode Closequote enc uni) else
                          if(name == "declarator") then return (LiteralNode Declarator enc uni) else
                           if(name == "junk") then return (LiteralNode Junk enc uni) else
                            if(name == "opener") then return (LiteralNode Opener enc uni) else 
                             if(name == "openquote") then return (LiteralNode Openquote enc uni) else 
                              if(name == "operator") then return (LiteralNode Operator enc uni) else
                               if(name == "punct") then return (LiteralNode Punct enc uni) else
                                if(name == "sigil") then return (LiteralNode Sigil enc uni) else
                                 if(name == "text") then return (LiteralNode Text enc uni) else
                                  if(name == "token") then return (LiteralNode Token enc uni) else
                                   return (LiteralNode UnknownLit enc uni)
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

{-
hashConstKey :: P5AST -> P5AST
hashConstKey (P5AST kids) = (P5AST (map hashConstKey kids))
hashConstKey (Condmod kids) = (Condmod (map hashConstKey kids))
hashConstKey (Listelem kids) = (Listelem (map hashConstKey kids))
hashConstKey (Op_helem kids) = (Op_helem ([(changeSigildp (head kids))] ++ (map changeOpenCloseAngle (tail kids))))

changeSigildp :: P5AST -> P5AST
changeSigildp (Op_rv2hv kids) = (Op_rv2hv (map changeSigildp kids))
changeSigildp (Sigil enc uni) = (Sigil enc ("%" ++ (tail uni)))


changeOpenCloseAngle :: P5AST -> P5AST
changeOpenCloseAngle (Opener enc uni) = (Opener enc "<")
changeOpenCloseAngle (Closer enc uni) = (Closer enc ">")
-}

--Translates =~ -> ~~ for using regexs in P6
equalTildeToTildeTilde :: P5AST -> P5AST
equalTildeToTildeTilde (LiteralNode Operator enc "=~") = (LiteralNode Operator enc "~~")
equalTildeToTildeTilde (AbstractNode atype kids) = (AbstractNode atype (map equalTildeToTildeTilde  kids))
equalTildeToTildeTilde (LiteralNode atype enc uni) = (LiteralNode atype enc uni)





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
                             print (AbstractNode P5AST dirs);
                             hClose inHandle;
                             printTree outHandle (translate (AbstractNode P5AST dirs));
                             hClose outHandle;
                             putStrLn "Finished"