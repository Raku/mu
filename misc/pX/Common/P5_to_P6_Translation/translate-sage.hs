{-This file is NO LONGER USED.
Development has moved to ASTTranslate.hs, ASTDefinition.hs, ASTParser.hs and ASTUtil.hs
for ease of use. -}

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

{-------------
nodeNamer is parsec parser that parses nodes,
recursivley parsing child nodes. It has two distinct cases,
one for nodes with kids, one for all other nodes.
---------------}
nodeNamer :: Int -> Parser P5AST
nodeNamer indent = do
    count indent space
    choice [hereDoc indent, withKids indent , noKids indent , blank indent]

blank :: Int -> Parser P5AST
blank indent = do
    try (string "- ''")
    newline
    return (LiteralNode Junk "1" "")

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
            "op_close"      -> Op_close
            "op_cond_expr"  -> Op_cond_expr
            "op_const"      -> Op_const
            "op_ftdir"      -> Op_ftdir
            "op_gv"         -> Op_gv
            "op_helem"      -> Op_helem
            "op_iter"       -> Op_iter
            "op_leave"      -> Op_leave
            "op_leaveloop"  -> Op_leaveloop
            "op_length"     -> Op_length
            "op_lineseq"    -> Op_lineseq
            "op_list"       -> Op_list
            "op_match"      -> Op_match
            "op_method"     -> Op_method
            "op_not"        -> Op_not
            "op_null"       -> Op_null
            "op_padav"      -> Op_padav
            "op_padsv"      -> Op_padsv
            "op_print"      -> Op_print
            "op_pushmark"   -> Op_pushmark
            "op_readline"   -> Op_readline
            "op_require"    -> Op_require
            "op_rv2av"      -> Op_rv2av
            "op_rv2gv"      -> Op_rv2gv
            "op_rv2hv"      -> Op_rv2hv
            "op_rv2sv"      -> Op_rv2sv
            "op_sassign"    -> Op_sassign
            "op_split"      -> Op_split
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
    let con = case name of
            "closer"        -> Closer
            "closequote"    -> Closequote
            "declarator"    -> Declarator
            "junk"          -> Junk
            "opener"        -> Opener
            "openquote"     -> Openquote
            "operator"      -> Operator
            "punct"         -> Punct
            "remod"         -> Remod
            "sigil"         -> Sigil
            "text"          -> Text
            "token"         -> Token
            _               -> UnknownLit 
    return $ LiteralNode con enc uni

hereDoc :: Int -> Parser P5AST
hereDoc indent = do
    try (string "- !perl/P5AST::heredoc") <?> "Heredoc decleration";
    newline
    spaces
    string "doc: !perl/P5AST::"
    doc <- manyTill anyChar space
    newline
    spaces
    string "kids: "
    newline
    kids <- many (try (nodeNamer (indent+4)))
    spaces
    string "end: !perl/p5::closequote "
    newline
    spaces
    string "enc: "
    endenc <- manyTill anyChar newline
    spaces
    string "uni: "
    enduni <- uniBlock (indent + 4) 
    spaces
    string "start: !perl/p5::openquote "
    newline
    spaces
    string "enc: "
    startenc <- manyTill anyChar newline
    spaces
    string "uni: "
    startuni <- uniBlock (indent + 4)
    return (Heredoc (LiteralNode Openquote startenc startuni) (LiteralNode Closequote endenc enduni) kids)
    
    

    
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
parseInput = do
    sequence_ (replicate 3 $ manyTill anyToken newline)
    names <- many (nodeNamer 2)
    eof 
    return names

getRidOfExtraSlashes :: String -> String
getRidOfExtraSlashes [] = []
getRidOfExtraSlashes inSt = if (and [((head inSt)=='\\'), ((head (tail inSt))=='\n')]) then ('\n':(getRidOfExtraSlashes (drop 2 inSt))) else ((head inSt):(getRidOfExtraSlashes (tail inSt)))

{-Function to handle escaped characters in a string scanned from input
For example, if the string "blah\n" is scanned, it ends up being represented as
"blah\\n". This function parses that newline into a literal newline.-}
makeLiterals :: String -> String
makeLiterals [] = []
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
printTree outFile (Heredoc start end kids) = do (printTree outFile start)
                                                printTree outFile (AbstractNode P5AST kids)
                                                printTree outFile end 
                                               

--Wrapper function to apply all translations in order
translate :: P5AST -> String -> P5AST
translate tree options= if (options == "Oo") then (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (regexSubstitutionTranslation tree))))))))))) else
                                                    (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (regexSubstitutionTranslation tree))))))))

foreachTranslation :: P5AST -> P5AST
foreachTranslation (AbstractNode Op_leaveloop kids) = if (isIn (LiteralNode Token "1" "foreach") kids) then 
                                                        if ((getLType (head kids))==(getLType (LiteralNode Junk "1" ""))) then (AbstractNode Op_leaveloop ((head kids):(extractKids (newForeach (map foreachTranslation kids))))) 
                                                           else (newForeach (map foreachTranslation kids))
                                                       else (AbstractNode Op_leaveloop (map foreachTranslation kids))
foreachTranslation (AbstractNode atype kids) = (AbstractNode atype (map foreachTranslation kids))
foreachTranslation (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
foreachTranslation (Heredoc start end kids) = (Heredoc start end kids)

newForeach :: [P5AST] -> P5AST
newForeach [] = (AbstractNode UnknownAbs [])
newForeach kids =  if (and [(isIn (AbstractNode Op_padsv []) kids),(isIn (AbstractNode Op_padav []) kids)]) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_padav []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Declarator "1" "my"),(extractNodetype (AbstractNode Op_padsv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else
                    if (and [(isIn (AbstractNode Op_padsv []) kids),(isIn (AbstractNode Op_list []) kids)]) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "1" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "1" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Declarator "1" "my"),(extractNodetype (AbstractNode Op_padsv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else 
                      if (isIn (AbstractNode Op_rv2av []) kids) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2av []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2gv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else
                        if (isIn (AbstractNode Op_list []) kids) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "1" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "1" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2gv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else (AbstractNode UnknownAbs [])

extractNodetype :: P5AST -> [P5AST] -> P5AST
extractNodetype _ [] = (AbstractNode UnknownAbs [])
extractNodetype node nlist = if (matchWithoutEnc node (head nlist)) then (head nlist) else (extractNodetype node (tail nlist))

closeToMethod :: P5AST -> P5AST
closeToMethod (AbstractNode Op_close kids) = (AbstractNode Op_close (changeCloseMethod kids))
closeToMethod (AbstractNode atype kids) = (AbstractNode atype (map closeToMethod kids))
closeToMethod (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
closeToMethod (Heredoc start end kids) = (Heredoc start end kids)

changeCloseMethod :: [P5AST] -> [P5AST]
changeCloseMethod [] = []
changeCloseMethod nlist = if (matchWithoutEnc (head nlist) (AbstractNode Op_rv2gv [])) then (extractKids (head (extractKids (head nlist))))++[(LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "close")])])] else
                            if (matchWithoutEnc (head nlist) (AbstractNode Op_gv [])) then [(LiteralNode Sigil "1" ("$"++(extractUni (head (extractKids (head (extractKids (head nlist)))))))),(LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "close")])])] else
                              (changeCloseMethod (tail nlist))

extractKids :: P5AST -> [P5AST]
extractKids (AbstractNode atype kids) = kids
extractKids (Heredoc start end kids) = kids
extractKids (LiteralNode _ _ _) = []

toWords :: P5AST -> P5AST
toWords (AbstractNode Op_split kids) = if (and [(isIn (AbstractNode Op_const []) kids),(isInSequence [(LiteralNode Openquote "1" "'"), (LiteralNode Text "1" " "), (LiteralNode Closequote "1" "'")] (extractKids (getConst kids)))]) then (AbstractNode Op_split [(getSecondArg kids), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "words")])])])
                                          else (AbstractNode Op_split (map toWords kids))
toWords (AbstractNode atype kids) = (AbstractNode atype (map toWords kids))
toWords (Heredoc start end kids) = (Heredoc start end kids)
toWords (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

getConst :: [P5AST] -> P5AST
getConst [] = (AbstractNode UnknownAbs [])
getConst nlist = if (matchWithoutEnc (head nlist) (AbstractNode Op_const [])) then (head nlist) else (getConst (tail nlist))

getSecondArg :: [P5AST] -> P5AST
getSecondArg [] = (AbstractNode UnknownAbs [])
getSecondArg list = if (matchWithoutEnc (head list) (AbstractNode Listelem [])) then (dropComma (head list)) else (getSecondArg (tail list))

dropComma :: P5AST -> P5AST
dropComma (AbstractNode Listelem kids) = (head (tail kids))
dropComma (AbstractNode atype kids) = (AbstractNode atype kids)
dropComma (Heredoc start end kids) = (Heredoc start end kids)
dropComma (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

splitQuotes :: P5AST -> P5AST
splitQuotes (AbstractNode Op_split kids) = (AbstractNode Op_split (join (map toSlashQuotes kids)))
splitQuotes (AbstractNode atype kids) = (AbstractNode atype (map splitQuotes kids))
splitQuotes (Heredoc start end kids) = (Heredoc start end kids)
splitQuotes (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

toSlashQuotes :: P5AST -> [P5AST]
toSlashQuotes (AbstractNode Op_const kids) = [(LiteralNode Openquote "1" "/"), (extractText kids),(LiteralNode Closequote "1" "/")]
toSlashQuotes (Heredoc start end kids) = [(Heredoc start end kids)]
toSlashQuotes (LiteralNode atype enc uni) = [(LiteralNode atype enc uni)]
toSlashQuotes (AbstractNode atype kids) = [(AbstractNode atype kids)]

extractText :: [P5AST] -> P5AST
extractText [] = (LiteralNode Text "1" "")
extractText kids = if ((getLType (head kids))==Text) then (head kids) else (extractText (tail kids)) 

getLType :: P5AST -> LitType
getLType (AbstractNode sometype kids) = UnknownLit
getLType (Heredoc start end kids) = UnknownLit
getLType (LiteralNode sometype enc uni) = sometype

join :: [[P5AST]] -> [P5AST]
join [] = []
join lists = (head lists)++(join (tail lists))

joinString :: [String] -> String
joinString [] = []
joinString strs = (head strs)++(joinString (tail strs))

lengthToMethod :: P5AST -> P5AST
lengthToMethod (AbstractNode Op_length kids) = (toCharMethod kids)
lengthToMethod (AbstractNode atype kids) = (AbstractNode atype (map lengthToMethod kids))
lengthToMethod (Heredoc start end kids) = (Heredoc start end kids)
lengthToMethod (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

toCharMethod :: [P5AST] -> P5AST
toCharMethod [] = (AbstractNode UnknownAbs [])
toCharMethod kids = if (matchWithoutEnc (head kids) (LiteralNode Opener "1" "(")) then (AbstractNode Op_length [(head (tail kids)), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "chars")])])]) else
                     (toCharMethod (tail kids))

{-Translates split calls on a regex with an explicit match (i.e. split(/blah/m, $something) to no longer
use the /m which now happens immediately. -}
splitOnMatchTranslate :: P5AST -> P5AST
splitOnMatchTranslate (AbstractNode Op_split kids) = (AbstractNode Op_split (map removeMModifier kids))
splitOnMatchTranslate (AbstractNode atype kids) = (AbstractNode atype (map splitOnMatchTranslate kids))
splitOnMatchTranslate (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
splitOnMatchTranslate (Heredoc start end kids) = (Heredoc start end kids)

{-Removes the m modifier from a regex-}
removeMModifier :: P5AST -> P5AST
removeMModifier (LiteralNode Openquote enc "m/") = (LiteralNode Openquote enc "/")
removeMModifier (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
removeMModifier (AbstractNode atype kids) = (AbstractNode atype kids)
removeMModifier (Heredoc start end kids) = (Heredoc start end kids)

readlineTranslate :: P5AST -> P5AST
readlineTranslate (AbstractNode Op_readline kids) = (AbstractNode Op_readline [(LiteralNode Sigil "1" ('$':(tail (reverse (tail (reverse (extractUni (head kids)))))))), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "readline")])])])
readlineTranslate (AbstractNode atype kids) = (AbstractNode atype (map readlineTranslate kids))
readlineTranslate (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
readlineTranslate (Heredoc start end kids) = (Heredoc start end kids)

extractUni :: P5AST -> String
extractUni (LiteralNode _ _ uni) = uni
extractUni (AbstractNode _ _) = "" 
extractUni (Heredoc _ _ _) = ""

{-Translations for substitution regexs.-}
regexSubstitutionTranslation :: P5AST -> P5AST
regexSubstitutionTranslation (AbstractNode Op_subst kids) = if (isIn (LiteralNode Closequote "1" "/g") kids) then (AbstractNode Op_subst (map equalTildeToTildeTilde (map substitutionGlobal kids)))
                                                               else (AbstractNode Op_subst (map equalTildeToTildeTilde kids))
regexSubstitutionTranslation (AbstractNode atype kids) = (AbstractNode atype (map regexSubstitutionTranslation kids))
regexSubstitutionTranslation (LiteralNode atype enc uni) = (LiteralNode atype enc uni) 
regexSubstitutionTranslation (Heredoc start end kids) = (Heredoc start end kids)


{-Translates =~ -> ~~ for using regexs with s/ in P6
The name of the function is a bit long, but it won't be called often
and at least it's very descriptive -}
equalTildeToTildeTilde :: P5AST -> P5AST
equalTildeToTildeTilde (LiteralNode Operator enc "=~") = (LiteralNode Operator enc "~~")
equalTildeToTildeTilde (AbstractNode atype kids) = (AbstractNode atype kids)
equalTildeToTildeTilde (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
equalTildeToTildeTilde (Heredoc start end kids) = (Heredoc start end kids)

{-Added changes for when a substitution is global -}
substitutionGlobal :: P5AST -> P5AST
substitutionGlobal (LiteralNode Openquote enc "s/") = (LiteralNode Openquote enc "s:P5:g/")
substitutionGlobal (LiteralNode Closequote enc "/g") = (LiteralNode Closequote enc "/")
substitutionGlobal (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
substitutionGlobal (AbstractNode atype kids) = (AbstractNode atype kids)
substitutionGlobal (Heredoc start end kids) = (Heredoc start end kids)


{- Function that converts conditional return statements (i.e. "a ? b : c") into a P5 form
a ?? b !! c.  No Context needed, if ? or : is ever a P5 operator, it's in one of these statements-}
conditionalExpression :: P5AST -> P5AST
conditionalExpression (LiteralNode Punct enc "?") = (LiteralNode Punct enc "??")
conditionalExpression (LiteralNode Punct enc ":") = (LiteralNode Punct enc "!!")
conditionalExpression (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
conditionalExpression (AbstractNode atype kids) = (AbstractNode atype (map conditionalExpression kids))
conditionalExpression (Heredoc start end kids) = (Heredoc start end kids)

{-Changes to arrays with keys, namely $array[i] -> @array[i]-}
arrayKey :: P5AST -> P5AST
arrayKey (AbstractNode Op_aelem kids) = if (isIn (AbstractNode Op_rv2av []) kids) then (AbstractNode Op_aelem (map arrayKeyChanges kids))
                                           else (AbstractNode Op_aelem (map arrayKey kids))
arrayKey (AbstractNode atype kids) = (AbstractNode atype (map arrayKey kids))
arrayKey (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
arrayKey (Heredoc start end kids) = (Heredoc start end (map arrayKey kids))

{-Actually applies the changes needed for the arrayKey function-}
arrayKeyChanges :: P5AST -> P5AST
arrayKeyChanges (AbstractNode Op_rv2av kids) = (AbstractNode Op_rv2av (map scalarSigilToArraySigil kids))
arrayKeyChanges (LiteralNode Sigil enc uni) = (scalarSigilToArraySigil (LiteralNode Sigil enc uni))
arrayKeyChanges (AbstractNode atype kids) = (AbstractNode atype kids)
arrayKeyChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
arrayKeyChanges (Heredoc start end kids) = (Heredoc start end kids)

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
hashKey (Heredoc start end kids) = (Heredoc start end (map hashKey kids))

{-Actually applie changes for hashKey-}
hashChanges :: P5AST -> P5AST
hashChanges (AbstractNode Op_rv2hv kids) = (AbstractNode Op_rv2hv (map scalarSigilToHashSigil kids))
hashChanges (AbstractNode atype kids) = (AbstractNode atype kids)
hashChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
hashChanges (Heredoc start end kids) = (Heredoc start end kids)

{-Additional changes for when a has has a constant key ({word}-><word>)-}
constHashChanges :: P5AST -> P5AST
constHashChanges (LiteralNode Opener enc "{") = (LiteralNode Opener enc "<")
constHashChanges (LiteralNode Closer enc "}") = (LiteralNode Closer enc ">")
constHashChanges (AbstractNode Op_rv2hv kids) = (AbstractNode Op_rv2hv (map scalarSigilToHashSigil kids))
constHashChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
constHashChanges (AbstractNode atype kids) = (AbstractNode atype kids)
constHashChanges (Heredoc start end kids) = (Heredoc start end kids)

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

isInSequence :: [P5AST] -> [P5AST] -> Bool
isInSequence _ [] = True
isInSequence [] _ = False
isInSequence nodes list = if (allMatch nodes list) then True else (isInSequence (tail nodes) list) 

allMatch :: [P5AST] -> [P5AST] -> Bool
allMatch [] [] = True
allMatch _ [] = False
allMatch [] _ = False
allMatch list1 list2 = if (matchWithoutEnc (head list1) (head list2)) then (allMatch (tail list1) (tail list2)) else False

{-Matches nodes based on type (Abstract or Literal) subtype (Junk, Op_leave, PNothing, etc)
and (in the case of literal nodes) on the uni field. Used in the above search functions.-}
matchWithoutEnc :: P5AST -> P5AST -> Bool
matchWithoutEnc (LiteralNode type1 _ uni1) (LiteralNode type2 _ uni2) = if (and [(uni1==uni2), (type1==type2)]) then True else False
matchWithoutEnc (AbstractNode type1 kids1) (AbstractNode type2 kids2) = if (type1 == type2) then True else False
matchWithoutEnc (Heredoc start1 end1 kids1) (Heredoc start2 end2 kids2) = if (and [(matchWithoutEnc start1 start2),(matchWithoutEnc end1 end2)]) then True else False
matchWithoutEnc _ _ = False

getModifiers :: [String] -> String
getModifiers [] = "0"
getModifiers args = if (and [('-'==(head (head args))),("Oo"==(tail (head args)))]) then "1" else (head args)

{-
A main function to parse a file containing a tree and output the contents to another file
Useage: mainParse inFile outFile options
options is a string with optional information, the only current option is "Oo" which applies more Object-oriented
translations then are strictly needed, such as $fh.close instead of close($fh)
-}
mainParse :: FilePath -> FilePath -> String-> IO ()
mainParse inName outName options= do
    inHandle    <- openFile inName ReadMode
    input       <- hGetContents inHandle
    outHandle   <- openFile outName WriteMode
    -- putStrLn ("DEBUG: got input " ++ input)
    let dirs = case parse parseInput "stdin" input of
            Left err -> error $ "\nError:\n" ++ show err
            Right result -> result
    putStrLn "DEBUG: parsed:";
    print (AbstractNode P5AST dirs)
    hClose inHandle
    printTree outHandle (translate (AbstractNode P5AST dirs) options)
    hClose outHandle
    putStrLn "Finished"
