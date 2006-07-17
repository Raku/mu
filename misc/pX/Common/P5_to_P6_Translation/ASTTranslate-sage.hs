module Main where
{-------------------------
"This is my Universal Translator. It could have been my greatest invention, but it translates everything into an incomprehensible dead language."
-Professor Hubert J. Farnsworth, Futurama

This module is the main guts of the Perl5 -> Perl6 Translator. Additional pieces can be found in:
ASTUtil - Utility Functions, mostly dealing with the P5AST
ASTParser - the Parsec parser for the yaml format AST of a P5 AST (the AST probably comes from MADSKILLS)
ASTDefinition - The AST datatype decleration. 


This file is designed to be compiled with GHC. The command
  $ ghc --make -o translate ASTTranslate.hs
usually works. 


The compiled version (let's call it 'translate') is used like so:
  $./translate [-Oo -V -U] inFile outFile
the -Oo, -U and -V switches are optional, and have these effects:

-Oo: Heavy object orientation. If there's an available (but optional) Oo translation it does it, such
as translating close($fh) to $fh.close.

-V: Verbose. The translator prints the entire translated AST to STDOUT. Useful for debugging small programs,
but not much else. For long files, echoing the entire tree to STDOUT may take more time then parsing.

-U: Unknown Debug. Whenever an unknown node is encountered during printing, prints "UnknownAbs" or
"UnknownLit" to the file every place an unknown node is encountered. Also Echoes "UNKNOWN: UnknownAbs"
or "UNKNOWN: UnknownLit" to STDOUT every time an unknown is encountered.


At this point, all code is highly unstable. Use at your own risk (but help welcome!).

Originally created by Sage LaTorra for Summer of Code 2006.
--------------------------}


import ASTUtil
import ASTParser
import ASTDefinition
import IO hiding (try)
import Text.ParserCombinators.Parsec
import System(getArgs)



--Wrapper function to apply all translations in order
--It's pretty ugly, which is why there's a need for a wrapper function
translate :: P5AST -> String -> P5AST
translate tree options= if ('o' `elem` options) then (regexInternals (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde (regexSubstitutionTranslation tree))))))))))))) else
                                                    (regexInternals (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde (regexSubstitutionTranslation tree))))))))))

regexInternals :: P5AST -> P5AST
regexInternals (AbstractNode Op_subst kids) = (AbstractNode Op_subst (changeSInternals kids))
regexInternals (AbstractNode atype kids) = (AbstractNode atype (map regexInternals kids))
regexInternals (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

changeSInternals :: [P5AST] -> [P5AST]
changeSInternals [] = []
changeSInternals kids =  if ((length kids) >= 3) then if (and [((head ((extractUni (head kids))++" "))=='s'),(matchOnType (head kids) (LiteralNode Openquote "" "")),(matchOnType (head (tail kids)) (LiteralNode Text "" "")),(matchOnType (head (drop 2 kids)) (LiteralNode Closequote "" ""))]) then do let regx = case parse regexString "Uni" (extractUni (head (tail kids))) of
                                                                                                                                                                                                                                                                                                                           Left err -> error $ "\nError:\n" ++ show err
                                                                                                                                                                                                                                                                                                                           Right result -> result
                                                                                                                                                                                                                                                                                                           [(head kids),(LiteralNode Text "1" regx),(head (drop 2 kids))]++(drop 3 kids)
                                                          else ((head kids):(changeSInternals (tail kids)))
                            else ((head kids):(changeSInternals (tail kids)))

regexString :: Parser String
regexString = do  strs <- (manyTill regexChar eof)
                  return (joinString strs)

regexChar :: Parser String
regexChar = choice[do{try(string "(space)"); return "<sp>"}, --Handle (space) -> <sp>
                   do{try(string "\\A"); return "^"},         -- \A -> ^
                   do{try(string "\\z"); return "$"},         -- \z -> $
                   do{try(string "\\Z"); return "\\n?$"},      -- \Z -> \n?$
                   do{try(string "\\n"); return "\\c[LF]"},    -- \n -> \c[LF]
                   do{try(string "\\r?\\n"); return "\\n"},     -- \r?\n -> \n
                   do{try(string "[^\\n]"); return "\\C[LF]"}, -- [^\n] -> \C[LF]
                   do{try(string "\\a"); return "\\c[BEL]"},   -- \a -> \c[BEL]
                   do{try(string "\\N{"); val <- (manyTill anyToken (char '}')); return ("\\c["++val++"]")}, -- \N{CENT SIGN} -> \c[CENT SIGN]
                   do{try(string "[^\\N{"); val <- (manyTill anyToken (string "}]")); return ("\\C["++val++"]")}, -- [^\N{CENT SIGN}] -> \C[CENT SIGN]
                   do{try(string "[^\\t]"); return "\\T"},     -- [^\t] -> \T
                   do{try(string "[^\\r]"); return "\\R"},     -- [^\r] -> \R
                   do{try(string "[^\\f]"); return "\\F"},     -- [^\f] -> \F
                   do{try(string "[^\\e]"); return "\\E"},     -- [^\e] -> \E
                   do{char <- anyToken; return (char:"")}]

topicSplit :: P5AST -> P5AST
topicSplit (AbstractNode Op_split kids) = (AbstractNode Op_split (topicMethod kids))
topicSplit (AbstractNode atype kids) = (AbstractNode atype (map topicSplit kids))
topicSplit (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
topicSplit (Heredoc start end kids) = (Heredoc start end (map topicSplit kids))

topicMethod :: [P5AST] -> [P5AST]
topicMethod [] = []
topicMethod kids = if (not (isIn (AbstractNode Listelem []) kids)) then [(LiteralNode Junk "" "")] else kids

--translates foreach loops:
--foreach $foo (@bar) {...} becomes foreach @bar -> $foo {...}
foreachTranslation :: P5AST -> P5AST
foreachTranslation (AbstractNode Op_leaveloop kids) = if (isIn (LiteralNode Token "1" "foreach") kids) then 
                                                        if ((getLType (head kids))==(getLType (LiteralNode Junk "1" ""))) then (AbstractNode Op_leaveloop ((head kids):(extractKids (newForeach (map foreachTranslation kids))))) 
                                                           else (newForeach (map foreachTranslation kids))
                                                       else (AbstractNode Op_leaveloop (map foreachTranslation kids))
foreachTranslation (AbstractNode atype kids) = (AbstractNode atype (map foreachTranslation kids))
foreachTranslation (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
foreachTranslation (Heredoc start end kids) = (Heredoc start end kids)

--Does the dirty work of actual translation on foreach
newForeach :: [P5AST] -> P5AST
newForeach [] = (AbstractNode UnknownAbs [])
newForeach kids =  if (and [(isIn (AbstractNode Op_padsv []) kids),(isIn (AbstractNode Op_padav []) kids)]) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_padav []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Declarator "1" "my"),(extractNodetype (AbstractNode Op_padsv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else
                    if (and [(isIn (AbstractNode Op_padsv []) kids),(isIn (AbstractNode Op_list []) kids)]) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "1" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "1" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Declarator "1" "my"),(extractNodetype (AbstractNode Op_padsv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else 
                      if (isIn (AbstractNode Op_rv2av []) kids) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2av []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2gv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else
                        if (isIn (AbstractNode Op_list []) kids) then (AbstractNode Op_leaveloop [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "1" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "1" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2gv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)]) else (AbstractNode UnknownAbs [])

--Changes a split on a sinle space into a call to the .words method
toWords :: P5AST -> P5AST
toWords (AbstractNode Op_split kids) = if (and [(isIn (AbstractNode Op_const []) kids),(isInSequence [(LiteralNode Openquote "1" "'"), (LiteralNode Text "1" " "), (LiteralNode Closequote "1" "'")] (extractKids (extractNodetype (AbstractNode Op_const []) kids)))]) then (AbstractNode Op_split [(getSecondArg kids), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "words")])])])
                                          else (AbstractNode Op_split (map toWords kids))
toWords (AbstractNode atype kids) = (AbstractNode atype (map toWords kids))
toWords (Heredoc start end kids) = (Heredoc start end kids)
toWords (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Changes close($fh) to $fh.close
--Only applied when -Oo
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

--Changes the quote type of regexs in a split.
splitQuotes :: P5AST -> P5AST
splitQuotes (AbstractNode Op_split kids) = (AbstractNode Op_split (join (map toSlashQuotes kids)))
splitQuotes (AbstractNode atype kids) = (AbstractNode atype (map splitQuotes kids))
splitQuotes (Heredoc start end kids) = (Heredoc start end kids)
splitQuotes (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Changes single quotes to slashes.
toSlashQuotes :: P5AST -> [P5AST]
toSlashQuotes (AbstractNode Op_const kids) = [(LiteralNode Openquote "1" "/"), (extractText kids),(LiteralNode Closequote "1" "/")]
toSlashQuotes (Heredoc start end kids) = [(Heredoc start end kids)]
toSlashQuotes (LiteralNode atype enc uni) = [(LiteralNode atype enc uni)]
toSlashQuotes (AbstractNode atype kids) = [(AbstractNode atype kids)]

--Changes length($blah) to $blah.chars with the help of toCharMethod
lengthToMethod :: P5AST -> P5AST
lengthToMethod (AbstractNode Op_length kids) = (toCharMethod kids)
lengthToMethod (AbstractNode atype kids) = (AbstractNode atype (map lengthToMethod kids))
lengthToMethod (Heredoc start end kids) = (Heredoc start end kids)
lengthToMethod (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Actually performs change to method call
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

--This function does all the work of tranlsating <FH> to $fh.readline.
--When an Op_readline is found, it's replaced with a new Op_readling consisting
--of a .readline method call from the filehandle that was being read.
readlineTranslate :: P5AST -> P5AST
readlineTranslate (AbstractNode Op_readline kids) = (AbstractNode Op_readline [(LiteralNode Sigil "1" ('$':(tail (reverse (tail (reverse (extractUni (head kids)))))))), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "readline")])])])
readlineTranslate (AbstractNode atype kids) = (AbstractNode atype (map readlineTranslate kids))
readlineTranslate (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
readlineTranslate (Heredoc start end kids) = (Heredoc start end kids)


{-Translations for substitution regexs.-}
regexSubstitutionTranslation :: P5AST -> P5AST
regexSubstitutionTranslation (AbstractNode Op_subst kids) = if (isIn (LiteralNode Closequote "1" "/g") kids) then (AbstractNode Op_subst (map equalTildeToTildeTilde (map substitutionGlobal kids)))
                                                               else (AbstractNode Op_subst (map equalTildeToTildeTilde kids))
regexSubstitutionTranslation (AbstractNode atype kids) = (AbstractNode atype (map regexSubstitutionTranslation kids))
regexSubstitutionTranslation (LiteralNode atype enc uni) = (LiteralNode atype enc uni) 
regexSubstitutionTranslation (Heredoc start end kids) = (Heredoc start end kids)


{-Translates =~ -> ~~ for using regexs in P6
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

{-
A main function to parse a file containing a tree and output the contents to another file
Useage: mainParse inFile outFile options
options is a string with optional information, created by the getModifiers function. mainParse only cares
if there's a 'v' in the options string, which starts verbose mode.
-}
mainParse :: FilePath -> FilePath -> String -> IO ()
mainParse inName outName options= do
    inHandle    <- openFile inName ReadMode
    input       <- hGetContents inHandle
    outHandle   <- openFile outName WriteMode
    putStrLn "--Perl 5 to Perl 6 Translation Utility--"
    putStrLn ("Translating: "++inName)
    putStrLn ("Output Target: "++outName)
    putStrLn ("Parsing Input File...")
    let ast = case parse parseInput "stdin" input of
            Left err -> error $ "\nError:\n" ++ show err
            Right result -> result
    putStrLn ("Translating...")
    let tree = (translate (AbstractNode P5AST ast) options)
    case [('v' `elem` options),('u' `elem` options)] of
             [True, True]   -> putStrLn "Printing with Verbose and Unknown Debug..."
             [False, True]  -> putStrLn "Printing with Unknown Debug..."
             [True, False]  -> putStrLn "Printing with Verbose..."
             [False, False] -> putStrLn "Printing..."
    case ('v' `elem` options) of
             True  -> do{putStrLn "VERBOSE: TREE(Translated):"; print tree; printTree outHandle tree options}
             False -> do{printTree outHandle tree options}
    hClose inHandle
    hClose outHandle
    putStrLn "Finished."

--The actual main function. Just grabs the args and parses them into a mainParse call.
main = do 
         inargs <- getArgs
         mainParse (getFirstFile inargs) (getSecondFile inargs) (getModifiers inargs)

--The following functions parse out the command line args into a good format for the mainParse call.

--getModifiers creates a string with an element for each command line switch.
getModifiers :: [String] -> String
getModifiers [] = "n"
getModifiers args = case (head args) of
                         "-Oo"    ->  ('o':(getModifiers (tail args)))
                         "-V"     ->  ('v':(getModifiers (tail args)))
                         "-U"     ->  ('u':(getModifiers (tail args)))
                         _        ->  ('n':(getModifiers (tail args)))

--getFirstFile (oddly enough) gets the first file (which will be the second to last argument). 
getFirstFile :: [String] -> String
getFirstFile [] = ""
getFirstFile args = (head (tail (reverse args))) 

--getSecondFile gets the second filename from the args. It should always be the last arument.
getSecondFile :: [String] -> String
getSecondFile [] = ""
getSecondFile args = (head (reverse args))