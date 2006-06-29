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
    | Op_aelem
    | Op_chdir
    | Op_const
    | Op_cond_expr
    | Op_entersub
    | Op_ftdir
    | Op_helem
    | Op_leave
    | Op_lineseq
    | Op_list
    | Op_match
    | Op_method
    | Op_not
    | Op_null
    | Op_print
    | Op_pushmark
    | Op_require
    | Op_rv2av
    | Op_rv2hv
    | Op_rv2sv
    | Op_sassign
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
    withKids indent <|> noKids indent -- <|> hereDoc indent --hereDoc not ready yet

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
            "condstate"     -> Condstate
            "listelem"      -> Listelem
            "nothing"       -> PNothing
            "op_aassign"    -> Op_aassign
            "op_aelem"      -> Op_aelem
            "op_chdir"      -> Op_chdir
            "op_cond_expr"  -> Op_cond_expr
            "op_const"      -> Op_const
            "op_ftdir"      -> Op_ftdir
            "op_helem"      -> Op_helem
            "op_leave"      -> Op_leave
            "op_lineseq"    -> Op_lineseq
            "op_list"       -> Op_list
            "op_match"      -> Op_match
            "op_method"     -> Op_method
            "op_not"        -> Op_not
            "op_null"       -> Op_null
            "op_print"      -> Op_print
            "op_pushmark"   -> Op_pushmark
            "op_rv2av"      -> Op_rv2av
            "op_rv2hv"      -> Op_rv2hv
            "op_rv2sv"      -> Op_rv2sv
            "op_sassign"    -> Op_sassign
            "op_subst"      -> Op_subst
            "op_stringify"  -> Op_stringify
            "package"       -> Package
            "peg"           -> Peg
            "statement"     -> Statement
            "sub"           -> Sub
            "ternary"       -> Ternary
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
            "declarator"    -> Declarator
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
{-------- hereDoc not ready yet
hereDoc :: Int -> Parser P5AST
hereDoc indent = do
    try (string "- !perl/P5AST::") <?> "P5AST decleration";
    name <- manyTill anyChar space
    newline
    spaces
    string "doc: "
    
---------}
    
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
        uni <- manyTill (manyTill anyToken newline) (try(newline)) <?> "uni block with chomp modifier";
        return (unlines (map (drop indent) uni))
    ,do uni <- manyTill anyToken newline <?> "uni string";
        --If the field is in quotes, strip the quotes by stripping the first character, 
        --reversing the string, stripping the first character, then reversing again
        return $ if (head uni `elem` "\"'")
            then (makeLiterals (reverse (tail (reverse (tail uni)))))
            else uni
    ]

--A wrapper for nodeNamer, to handle the junk at the beginning of the file.
parseInput :: Parser [P5AST]
parseInput = do
    sequence_ (replicate 3 $ manyTill anyToken newline)
    names <- many (nodeNamer 2)
    eof 
    return names

{-Function to handle escaped characters in a string scanned from input
For example, if the string "blah\n" is scanned, it ends up being represented as
"blah\\n". This function parses that newline into a literal newline.-}
makeLiterals :: String -> String
makeLiterals [] = []
makeLiterals inSt = if ((head inSt)=='\\') then if (head (tail inSt) == '"') then ('\"':(makeLiterals(drop 2 inSt))) else
                                                   if (head (tail inSt) == 'n') then ('\n':(makeLiterals(drop 2 inSt))) else
                                                     if (head (tail inSt) == 't') then ('\t':(makeLiterals(drop 2 inSt))) else
                                                    ('\\':(makeLiterals(drop 2 inSt)))
                      else ((head inSt):(makeLiterals (tail inSt)))

{- No longer a big big messy function to print all the different node types, 
now a slim function to print everything to a file.
Only two cases: LiteralNode and AbstractNode.
For a literal node, print the uni field.
For an abstract node, recursivley call printTree on the kids (if there are any).
All output is to a file
-}
printTree :: Handle -> P5AST -> IO ()
{------------ Uncomment this section to help find Unknown Nodes
printTree outFile (LiteralNode UnknownLit _ uni) = do{ hPutStr outFile "UnknownLit";
                                                       hPutStr outFile uni}
printTree outFile (AbstractNode UnknownAbs kids) = do{ hPutStr outFile "UnknownAbs";
                                                       printTree outFile (head kids);
                                                       printTree outFile (AbstractNode P5AST (tail kids))}
-------------------------------------------------------------}
printTree outFile (LiteralNode _ _ uni) = hPutStr outFile uni
printTree outFile (AbstractNode _ []) = hPutStr outFile ""
printTree outFile (AbstractNode _ kids) = do{ printTree outFile (head kids);
                                              printTree outFile (AbstractNode P5AST (tail kids))}

--Wrapper function to apply all translations in order
translate :: P5AST -> P5AST
translate tree = (conditionalExpression (arrayKey (hashKey (regexSubstitutionTranslation tree))))

{-Translations for substitution regexs.-}
regexSubstitutionTranslation :: P5AST -> P5AST
regexSubstitutionTranslation (AbstractNode Op_subst kids) = if (isIn (LiteralNode Closequote "1" "/g") kids) then (AbstractNode Op_subst (map equalTildeToTildeTilde (map substitutionGlobal kids)))
                                                               else (AbstractNode Op_subst (map equalTildeToTildeTilde kids))
regexSubstitutionTranslation (AbstractNode atype kids) = (AbstractNode atype (map regexSubstitutionTranslation kids))
regexSubstitutionTranslation (LiteralNode atype enc uni) = (LiteralNode atype enc uni) 


{-Translates =~ -> ~~ for using regexs with s/ in P6
The name of the function is a bit long, but it won't be called often
and at least it's very descriptive -}
equalTildeToTildeTilde :: P5AST -> P5AST
equalTildeToTildeTilde (LiteralNode Operator enc "=~") = (LiteralNode Operator enc "~~")
equalTildeToTildeTilde (AbstractNode atype kids) = (AbstractNode atype kids)
equalTildeToTildeTilde (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-Added changes for when a substitution is global -}
substitutionGlobal :: P5AST -> P5AST
substitutionGlobal (LiteralNode Openquote enc "s/") = (LiteralNode Openquote enc "s:P5:g/")
substitutionGlobal (LiteralNode Closequote enc "/g") = (LiteralNode Closequote enc "/")
substitutionGlobal (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
substitutionGlobal (AbstractNode atype kids) = (AbstractNode atype kids)


{- Function that converts conditional return statements (i.e. "a ? b : c") into a P5 form
a ?? b !! c.  No Context needed, if ? or : is ever a P5 operator, it's in one of these statements-}
conditionalExpression :: P5AST -> P5AST
conditionalExpression (LiteralNode Punct enc "?") = (LiteralNode Punct enc "??")
conditionalExpression (LiteralNode Punct enc ":") = (LiteralNode Punct enc "!!")
conditionalExpression (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
conditionalExpression (AbstractNode atype kids) = (AbstractNode atype (map conditionalExpression kids))

{-Changes to arrays with keys, namely $array[i] -> @array[i]-}
arrayKey :: P5AST -> P5AST
arrayKey (AbstractNode Op_aelem kids) = if (isIn (AbstractNode Op_rv2av []) kids) then (AbstractNode Op_aelem (map arrayKeyChanges kids))
                                           else (AbstractNode Op_aelem (map arrayKey kids))
arrayKey (AbstractNode atype kids) = (AbstractNode atype (map arrayKey kids))
arrayKey (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-Actually applies the changes needed for the arrayKey function-}
arrayKeyChanges :: P5AST -> P5AST
arrayKeyChanges (AbstractNode Op_rv2av kids) = (AbstractNode Op_rv2av (map scalarSigilToArraySigil kids))
arrayKeyChanges (LiteralNode Sigil enc uni) = (scalarSigilToArraySigil (LiteralNode Sigil enc uni))
arrayKeyChanges (AbstractNode atype kids) = (AbstractNode atype kids)
arrayKeyChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-$something->@something, used by the arrayKeyChanges function-}
scalarSigilToArraySigil :: P5AST -> P5AST
scalarSigilToArraySigil (LiteralNode Sigil enc uni) = (LiteralNode Sigil enc ('@':(tail uni)))
scalarSigilToArraySigil (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-Do changes to hashes with keys, such as $hash{word}->%hash<word> 
and $hash{$var}->%hash{$var}-}
hashKey :: P5AST -> P5AST
hashKey (AbstractNode Op_helem kids) = if (isInOrder [(AbstractNode Op_rv2hv []), (LiteralNode Opener "1" "{"), (AbstractNode Op_const []), (LiteralNode Closer "1" "}")] kids) 
                                              then (AbstractNode Op_helem (map constHashChanges kids)) else
                                              if (isInOrder [(AbstractNode Op_rv2hv []), (LiteralNode Opener "1" "{"), (LiteralNode Closer "1" "}")] kids)
                                                then (AbstractNode Op_helem (map hashChanges kids)) 
                                                else (AbstractNode Op_helem (map hashKey kids)) 
hashKey (AbstractNode atype kids) = (AbstractNode atype (map hashKey kids))
hashKey (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-Actually applie changes for hashKey-}
hashChanges :: P5AST -> P5AST
hashChanges (AbstractNode Op_rv2hv kids) = (AbstractNode Op_rv2hv (map scalarSigilToHashSigil kids))
hashChanges (AbstractNode atype kids) = (AbstractNode atype kids)
hashChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-Additional changes for when a has has a constant key ({word}-><word>)-}
constHashChanges :: P5AST -> P5AST
constHashChanges (LiteralNode Opener enc "{") = (LiteralNode Opener enc "<")
constHashChanges (LiteralNode Closer enc "}") = (LiteralNode Closer enc ">")
constHashChanges (AbstractNode Op_rv2hv kids) = (AbstractNode Op_rv2hv (map scalarSigilToHashSigil kids))
constHashChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
constHashChanges (AbstractNode atype kids) = (AbstractNode atype kids)

{-Function to change the sigil from a scalar ($something) to a hash (%something)-}
scalarSigilToHashSigil :: P5AST -> P5AST
scalarSigilToHashSigil (LiteralNode Sigil enc uni) = (LiteralNode Sigil enc ('%':(tail uni)))
scalarSigilToHashSigil (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

{-A simple search convenience function, returns true when the given node is in the list
matches based on type (Abstract or Literal) subtype (Junk, Op_leave, Sigil, etc.) and, in
the case of literal nodes, the uni part of the node.-}
isIn :: P5AST -> [P5AST] -> Bool
isIn _ [] = False
isIn node list = if (matchWithoutEnc (head list) node) then True else (isIn node (tail list))

{-Searches for the given list of nodes in another list
allows any number of nodes between the nodes being searched for
matches like isIn
Useage: isInOrder [NodesBeingSearchedFor] [SearchTarget]-}
isInOrder :: [P5AST] -> [P5AST] -> Bool
isInOrder [] [] = True
isInOrder _ [] = False
isInOrder [] _ = True
isInOrder nodes list = if (matchWithoutEnc (head list) (head nodes)) then (isInOrder (tail nodes) (tail list)) else (isInOrder nodes (tail list))

{-Matches nodes based on type (Abstract or Literal) subtype (Junk, Op_leave, PNothing, etc)
and (in the case of literal nodes) on the uni field. Used in the above search functions.-}
matchWithoutEnc :: P5AST -> P5AST -> Bool
matchWithoutEnc (LiteralNode type1 _ uni1) (LiteralNode type2 _ uni2) = if (and [(uni1==uni2), (type1==type2)]) then True else False
matchWithoutEnc (AbstractNode type1 kids1) (AbstractNode type2 kids2) = if (type1 == type2) then True else False
matchWithoutEnc _ _ = False


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
