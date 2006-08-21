module Main where
{-------------------------
"This is my Universal Translator. It could have been my greatest invention, but it translates everything into an incomprehensible dead language."
-Professor Hubert J. Farnsworth, Futurama

This module is the main guts of the Perl5 -> Perl6 Translator. Additional pieces can be found in:
ASTUtil - Utility Functions, mostly dealing with the P5AST
ASTParser - The Parsec parser for the yaml format AST of a P5 AST (the AST probably comes from MAD_SKILLS)
ASTDefinition - The AST datatype decleration. 


To build the translator, use
  $ make
to build Sage's personal copy (may be more up to date, but also less stable) use
  $ make sage

This file is designed to be compiled with GHC. The command
  $ ghc --make -o translate ASTTranslate.hs
works (but has not been widely tested). (The makefile is actually just an alias for ghc --make) 


The compiled version (let's call it 'translate') is used like so:
  $./translate [-Oo -V -U -R -N -S] inFile outFile
the switches are optional, and have these effects:

-Oo: Heavy object orientation. If there's an available (but optional) Oo translation it does it, such 
as translating close($fh) to $fh.close.

-V: Verbose. The translator prints the entire translated AST to STDOUT. Useful for debugging small programs, but not much else. For long files, echoing the entire tree to STDOUT may take more time then parsing.

-U: Unknown Debug. Whenever an unknown node is encountered during printing, prints "UnknownAbs" or "UnknownLit" to the file every place an unknown node is encountered. Also Echoes "UNKNOWN: UnknownAbs" or "UNKNOWN: UnknownLit" to STDOUT every time an unknown is encountered.

-R: Minimal regex changes. The translator will  not attempt to translate the Regex from Perl 5 syntax to Perl 6. It will instead apply the :Perl5 modifer to the regex and move the re mods (/g, /i, etc.) to the start (:g, :i, etc.). The only internal changes made are explicitly aliasing the captures, so surrounding code will never know the difference.

-N: No translation. The translator just parses the yaml file back into a perl 5 file. No other options matter with -N.

-S: Just turn off strict instead of trying to declare all variables properly.

Translations may run _slightly_ faster without the -Oo switch. Translations will almost _always_ take longer with -V (printing to STDOUT often takes longer then a full run, for large numbers of nodes). -U and -R should have little to no effect on speed (but if the AST contains unknown nodes with -U, the resulting code will be invlaid).

At this point, all code is highly unstable. Use at your own risk (but help welcome!).

Originally created by Sage LaTorra for Summer of Code 2006. Powered by Red Bull, Taco Bell, and old episodes of Futurama and Battlestar Galactica.
--------------------------}


import ASTUtil
import ASTParser
import ASTDefinition
import IO hiding (try)
import Text.ParserCombinators.Parsec
import System(getArgs)
import List



--Wrapper function to apply all translations in order
--It's pretty ugly, which is why there's a need for a wrapper function
translate :: P5AST -> String -> P5AST
translate tree options = if ('n' `elem` options) then (tree) else case [('o' `elem` options), ('r' `elem` options), ('s' `elem` options)] of
                               [True, False, False]   ->  (decWrapper (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (regexModifiers (regexOnce (scalarTranslate (hereDocTranslate (regexInternals (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree)))))))))))))))))))))
                               [True, True, False]  ->  (decWrapper (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (easyRegex (changeVarsInQuotes  (scalarTranslate (regexOnce (hereDocTranslate (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree))))))))))))))))))))
                               [False, False, False]  -> (decWrapper (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (regexModifiers (regexOnce (scalarTranslate (hereDocTranslate (regexInternals (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree))))))))))))))))))
                               [False, True, False] -> (decWrapper (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (easyRegex (regexOnce (scalarTranslate (hereDocTranslate (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree)))))))))))))))))
                               [True, False, True]   ->  (noStrict (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (regexModifiers (regexOnce (scalarTranslate (hereDocTranslate (regexInternals (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree)))))))))))))))))))))
                               [True, True, True]  ->  (noStrict (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (easyRegex (changeVarsInQuotes  (scalarTranslate (regexOnce (hereDocTranslate (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree))))))))))))))))))))
                               [False, False, True]  -> (noStrict (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (regexModifiers (regexOnce (scalarTranslate (hereDocTranslate (regexInternals (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree))))))))))))))))))
                               [False, True, True] -> (noStrict (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (easyRegex (regexOnce (scalarTranslate (hereDocTranslate (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree)))))))))))))))))


--A wrapper to grab the undeclared variable names and add them to the AST

decWrapper :: P5AST -> P5AST
decWrapper tree = (addDecs (getUndeclaredVars tree) tree)


--Declerations are added to the beginng of the tree. The first input is a list of names of variables to be declared, the second arg is the AST they are to be added to.

addDecs :: [String] -> P5AST -> P5AST
addDecs [] (AbstractNode atype kids)   = (AbstractNode atype kids)
addDecs vars (AbstractNode atype kids) = (addDecs (tail vars) (AbstractNode atype ((AbstractNode "statement" [(LiteralNode "declarator" "" "my"), (LiteralNode "junk" "" " "), (LiteralNode "sigil" "" (head vars)), (LiteralNode "token" "" ";"), (LiteralNode "junk" "" "\n")]):kids)))


--From a P5AST, this function gets a list of undeclared variables by comparing a list of all sigils and a list of declared vars, and returns the undeclared variables

getUndeclaredVars :: P5AST -> [String]
getUndeclaredVars tree = ((nub (getAllVars tree))\\(nub (getDeclaredVars tree)))


--This function resturn a list of ALL variable names. It _may_ (and likely will) return duplicates, nub-ing the results is a good idea.

getAllVars :: P5AST -> [String]
getAllVars (AbstractNode atype kids)     = (makeList (map getAllVars kids))
getAllVars (LiteralNode "sigil" enc uni) = [uni]
getAllVars (LiteralNode _ _ _)           = [""]
getAllVars (Heredoc _ _ _ _)             = [""]

--This function returns a list of all declared variables. It _may_ (and likely will) return duplicates, nub-ing the results is a good idea.

getDeclaredVars :: P5AST -> [String]
getDeclaredVars (AbstractNode atype kids) = if (isInSequenceType kids [(LiteralNode "declarator" "" ""),(LiteralNode "sigil" "" "")]) then [(getVar kids)] else (makeList (map getDeclaredVars kids))
getDeclaredVars (LiteralNode _ _ _)       = [""]
getDeclaredVars (Heredoc _ _ _ _)         = [""]


--From a list of nodes, this extracts the first declared variable.

getVar :: [P5AST] -> String
getVar []   = ""
getVar kids = if (matchOnType (head kids) (LiteralNode "declerator" "" "")) then (junkTillSigil kids) else (getVar (tail kids))


--Drop junk between a declarator and a sigil, since there will always be junk between them.

junkTillSigil :: [P5AST] -> String
junkTillSigil []   = ""
junkTillSigil kids = if (matchOnType (head kids) (LiteralNode "junk" "" "")) then (junkTillSigil (tail kids)) else (extractUni (head kids))


--Add 'no strict' to make it easier to translate. The nodes for "no strict;" are added to the beginning of the AST.

noStrict :: P5AST -> P5AST
noStrict (AbstractNode atype kids) = (AbstractNode atype ((AbstractNode "use" [(LiteralNode "junk" "" "\n"),(LiteralNode "operator" "" "no"), (AbstractNode "op_const" [(LiteralNode "junk" "" " "), (LiteralNode "token" "" "strict"), (LiteralNode "token" "" ";"),(LiteralNode "junk" "" "\n")])]):(kids)))


--Find any instance of print or printf that prints to a file and add a colon

filePrintChange :: P5AST -> P5AST
filePrintChange (AbstractNode "op_print" kids) = if (or [(isInSequence kids [(LiteralNode "operator" "" "print"),(AbstractNode "op_rv2gv" [])]), (isInSequence kids [(LiteralNode "operator" "" "print"),(LiteralNode "opener" "" "("),(AbstractNode "op_rv2gv" [])])]) then (AbstractNode "op_print" (addPrintColon kids)) else (AbstractNode "op_print" kids)
filePrintChange (AbstractNode "op_prtf" kids)  = if (or [(isInSequence kids [(LiteralNode "operator" "" "printf"),(AbstractNode "op_rv2gv" [])]), (isInSequence kids [(LiteralNode "operator" "" "printf"),(LiteralNode "opener" "" "("),(AbstractNode "op_rv2gv" [])])]) then (AbstractNode "op_print" (addPrintColon kids)) else (AbstractNode "op_print" kids)
filePrintChange (AbstractNode atype kids)      = (AbstractNode atype (map filePrintChange kids))
filePrintChange (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
filePrintChange (Heredoc doc start end kids)   = (Heredoc doc start end kids)


--Adds a colon after the filehandle argument of print or printf

addPrintColon :: [P5AST] -> [P5AST]
addPrintColon []   = []
addPrintColon kids = if (matchOnType (head kids) (AbstractNode "op_rv2gv" [])) then (head kids):(LiteralNode "token" "" ":"):(tail kids) else (head kids):(addPrintColon (tail kids))


--Finds sub declerations that match the names of things being exported and adds "is export" to them
--The first argument is the names of things that need to be exported, the second arg is the tree to have the nodes added to.

changeExportOkay :: [String] -> P5AST -> P5AST
changeExportOkay [] (AbstractNode "sub" kids)    = (AbstractNode "sub" kids)
changeExportOkay names (AbstractNode "sub" kids) = if (isIn (LiteralNode "token" "" (head names)) (extractKids (extractNodetype (AbstractNode "op_const" []) kids))) then (AbstractNode "sub" (addExportOkay kids)) else (changeExportOkay (tail names) (AbstractNode "sub" kids))
changeExportOkay names (AbstractNode atype kids) = (AbstractNode atype (map (changeExportOkay names) kids))
changeExportOkay _ (LiteralNode atype enc uni)   = (LiteralNode atype enc uni)
changeExportOkay _ (Heredoc doc start end kids)  = (Heredoc doc start end kids)


--Adds "is export" to anything that is @EXPORT_OK

addExportOkay :: [P5AST] -> [P5AST]
addExportOkay []   = []
addExportOkay kids = if (matchWithoutEnc (LiteralNode "opener" "" "{") (head kids)) then (LiteralNode "token" "" " is export"):(kids) else (head kids):(addExportOkay (tail kids))


--getExportOkay returns the names of everything assigned to @EXPORT_OK in a list of strings

getExportOkay :: P5AST -> [String]
getExportOkay (AbstractNode "op_aassign" kids) = case [(matchOnType (smartHead kids) (AbstractNode "op_list" [])), (matchWithoutEnc (smartHead (extractKids (smartHead (extractKids (smartHead kids))))) (LiteralNode "sigil" "" "@EXPORT_OK")), (isIn (LiteralNode "operator" "" "=") kids), (matchOnType (AbstractNode "quote" []) (smartHead (extractKids (extractNodetype (AbstractNode "op_list" []) (tail kids)))))] of
    [True, True, True, True] -> (extractExportsFromQW (extractKids (smartHead (extractKids (extractNodetype (AbstractNode "op_list" []) (tail kids))))))
    _                        -> []
getExportOkay (AbstractNode atype kids)        = (makeList (map getExportOkay kids))
getExportOkay (LiteralNode atype enc uni)      = []
getExportOkay (Heredoc doc start end kids)     = []


{-Changes things stored in @EXPORT to sub ... is export(:MANDATORY)-}

changeExports :: [String] -> P5AST -> P5AST
changeExports [] (AbstractNode "sub" kids)    = (AbstractNode "sub" kids)
changeExports names (AbstractNode "sub" kids) = if (isIn (LiteralNode "token" "" (head names)) (extractKids (extractNodetype (AbstractNode "op_const" []) kids))) then (AbstractNode "sub" (addExport kids)) else (changeExports (tail names) (AbstractNode "sub" kids))
changeExports names (AbstractNode atype kids) = (AbstractNode atype (map (changeExports names) kids))
changeExports _ (LiteralNode atype enc uni)   = (LiteralNode atype enc uni)
changeExports _ (Heredoc doc start end kids)  = (Heredoc doc start end kids)


{-Actually adds the "is export(:MANDATORY)" text. Since there isn't really a way to add that to a P5AST, it's just stuck in a Token node.-}

addExport :: [P5AST] -> [P5AST]
addExport []   = []
addExport kids = if (matchWithoutEnc (LiteralNode "opener" "" "{") (head kids)) then (LiteralNode "token" "" " is export(:MANDATORY)"):(kids) else (head kids):(addExport (tail kids))


{-This function finds any lines that assign to @EXPORT and gets the names stored in @EXPORT so that they can be handed to changeExports. The captured names are returned in a list of strings (with the & stripped off for functions).-}

getExports :: P5AST -> [String]
getExports (AbstractNode "op_aassign" kids) = case [(matchOnType (smartHead kids) (AbstractNode "op_list" [])), (matchWithoutEnc (smartHead (extractKids (smartHead (extractKids (smartHead kids))))) (LiteralNode "sigil" "" "@EXPORT")), (isIn (LiteralNode "operator" "" "=") kids), (matchOnType (AbstractNode "quote" []) (smartHead (extractKids (extractNodetype (AbstractNode "op_list" []) (tail kids)))))] of
    [True, True, True, True] -> (extractExportsFromQW (extractKids (smartHead (extractKids (extractNodetype (AbstractNode "op_list" []) (tail kids))))))
    _                        -> []
getExports (AbstractNode atype kids)        = (makeList (map getExports kids))
getExports (LiteralNode atype enc uni)      = []
getExports (Heredoc doc start end kids)     = []


--My own version of head that doesn't fail on an empty list. Used for some debugging purposes, mostly.

smartHead :: [P5AST] -> P5AST
smartHead []    = (AbstractNode "UnknownAbs" [])
smartHead alist = head alist


--Grabs exports from something of the form @EXPORT = qw{blah foo bar}

extractExportsFromQW :: [P5AST] -> [String]
extractExportsFromQW []   = []
extractExportsFromQW kids = if (matchOnType (LiteralNode "openquote" "" "") (head kids)) then (map (dropLeadingChar '&') (makeWords (extractUni (head (drop 1 kids))) [""])) else (extractExportsFromQW (tail kids))


--Translates "@array" -> "@array[]" and "%hash" -> "%hash{}", as well as
--other changes needed inside quotes ($array[1] -> @array[1], etc.)

changeVarsInQuotes :: P5AST -> P5AST
changeVarsInQuotes (LiteralNode "text" enc uni) = (LiteralNode "text" enc (runTextParser uni))
changeVarsInQuotes (AbstractNode atype kids)    = (AbstractNode atype (map changeVarsInQuotes kids))
changeVarsInQuotes (LiteralNode atype enc uni)  = (LiteralNode atype enc uni)
changeVarsInQuotes (Heredoc doc start end kids) = (Heredoc doc start end (map changeVarsInQuotes kids))


--Wrapper for the parser that translates arrays and hashes in strings.
--runTextParser now no longer fails terminally, if it fails it just returns it's input (after printing a message)

runTextParser :: String -> String
runTextParser instr = case parse textParser "text node" instr of
    Left err -> do{[putStrLn ("\nNon-Terminal Error:\n" ++ show err ++"\nIN:\n"++instr)]; instr}
    Right result -> result

--The actual parser that changes "@array" and "%hash" within text

textParser :: Parser String
textParser = do{ parts <- manyTill (choice[
        do{char '$'; name <- many alphaNum; choice[do{char '['; key <- manyTill anyToken (char ']'); return ("@"++name++"["++key++"]")}, do{char '{'; key <- manyTill anyToken (char '}'); return ("%"++name++"<"++key++">")}, return ("$"++name)]},
        do{char '@'; name <- many alphaNum; return ('@':name++"[]")},
        do{char '%'; name <- many alphaNum; return ('%':name++"{}")},
        do{this <- anyToken; return [this]}]) 
    eof; return (joinString parts)}


--Translates ?foo? to m:once/foo/ (and m?foo? to m:once/foo/)
--The order in which this is applied is very important, it should be applied
--BEFORE other regex modifiers (/i, for example) have been processed.

regexOnce :: P5AST -> P5AST
regexOnce (AbstractNode "op_match" kids) = (AbstractNode "op_match" (newOnce kids))
regexOnce (AbstractNode atype kids)      = (AbstractNode atype (map regexOnce kids))
regexOnce (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
regexOnce (Heredoc doc start end kids)   = (Heredoc doc start end kids)


--Changes the opening ? of a once regex to m:once/

newOnce :: [P5AST] -> [P5AST]
newOnce []   = []
newOnce kids = case [(matchWithoutEnc (head kids) (LiteralNode "openquote" "" "m?")), (matchWithoutEnc (head kids) (LiteralNode "openquote" "" "?"))] of
    [True, False] -> (LiteralNode "openquote" "" "m:once/"):(finalOnce (tail kids))
    [False, True] -> (LiteralNode "openquote" "" "m:once/"):(finalOnce (tail kids))
    _             -> (head kids):(newOnce (tail kids))


--Changes the closing ? to / in a once regex

finalOnce :: [P5AST] -> [P5AST]
finalOnce []   = []
finalOnce kids = case [(matchOnType (LiteralNode "openquote" "" "") (head kids)), ('?' `elem` (extractUni (head kids)))] of
    [True, True] -> (LiteralNode "openquote" "" ('/':(tail (extractUni (head kids))))):(tail kids)
    _            -> (head kids):(finalOnce (tail kids))


--Find places where the translation scalar @blah -> +@blah is needed

scalarTranslate :: P5AST -> P5AST
scalarTranslate (AbstractNode "op_scalar" kids) = (AbstractNode "op_scalar" (scalarToPlus kids))
scalarTranslate (AbstractNode atype kids)       = (AbstractNode atype (map scalarTranslate kids))
scalarTranslate (LiteralNode atype enc uni)     = (LiteralNode atype enc uni)
scalarTranslate (Heredoc doc start end kids)    = (Heredoc doc start end kids)


--Change a scalar function call to + symbol.

scalarToPlus :: [P5AST] -> [P5AST]
scalarToPlus []   = []
scalarToPlus kids = case (matchWithoutEnc (head kids) (LiteralNode "operator" "" "scalar")) of
    False -> (head kids):(scalarToPlus (tail kids))
    True  -> (LiteralNode "operator" "" "+"):(dropLeadingJunk (head (drop 1 kids))):(drop 2 kids)


--Removes leading junk 

dropLeadingJunk :: P5AST -> P5AST
dropLeadingJunk (LiteralNode atype enc uni)  = (LiteralNode atype enc uni)
dropLeadingJunk (AbstractNode atype kids)    = case (matchOnType (head kids) (LiteralNode "junk" "" "")) of
    True  -> (AbstractNode atype (drop 1 kids))
    False -> (AbstractNode atype kids)
dropLeadingJunk (Heredoc doc start end kids) = (Heredoc doc start end kids)


--Translates herdocs. Just changes the start...

hereDocTranslate :: P5AST -> P5AST
hereDocTranslate (Heredoc doc start end kids) = (Heredoc doc (changeHereDoc start) end kids)
hereDocTranslate (AbstractNode atype kids)    = (AbstractNode atype (map hereDocTranslate kids))
hereDocTranslate (LiteralNode atype enc uni)  = (LiteralNode atype enc uni)


--Change the syntax of heredocs

changeHereDoc :: P5AST -> P5AST
changeHereDoc (LiteralNode atype enc uni) = case ((head uni):(head (drop 1 uni)):[(head (drop 1 (drop 1 uni)))]) of
    "<<\"" -> (LiteralNode atype enc ("qq:to/"++(drop 3 uni)++"/"))
    "<<'"  -> (LiteralNode atype enc ("q:to/"++(drop 3 uni)++"/"))
    _      -> (LiteralNode atype enc ("qq:to/"++(drop 2 uni)++"/"))


--easyRegex just moves around the remods and then changes the captures

easyRegex :: P5AST -> P5AST
easyRegex (AbstractNode "op_subst" kids) = (AbstractNode "op_subst" (easyRegexSChanges kids))
easyRegex (AbstractNode "op_match" kids) = (AbstractNode "op_match" (easyRegexMChanges kids))
easyRegex (AbstractNode atype kids)      = (AbstractNode atype (map easyRegex kids))
easyRegex (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)


--Just get the relevent Remods and put them at the beginning (for a substitution regex)

easyRegexSChanges :: [P5AST] -> [P5AST]
easyRegexSChanges []   = []
easyRegexSChanges kids = case [(matchOnType (head kids) (LiteralNode "openquote" "" "")), ('g' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids)))] of
    [True, True, True] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:i:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    [True, True, False] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    [True, False, True] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:i:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    [True, False, False] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    _ -> (head kids):(easyRegexSChanges (tail kids))


--Just get the relevent Re mods and put them at the beginning (for a match regex)

easyRegexMChanges :: [P5AST] -> [P5AST]
easyRegexMChanges []   = []
easyRegexMChanges kids = case [(matchOnType (head kids) (LiteralNode "openquote" "" "")), ('g' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids)))] of
    [True, True, True] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:i:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    [True, True, False] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    [True, False, True] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:i:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    [True, False, False] -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
    _ -> (head kids):(easyRegexSChanges (tail kids))


--Change captures (and nothing else) in a regex

easyCapChanges :: P5AST -> P5AST
easyCapChanges (LiteralNode "text" enc uni) = (LiteralNode "text" enc (regexChangeCaptures uni))
easyCapChanges (LiteralNode atype enc uni)  = (LiteralNode atype enc uni)
easyCapChanges (AbstractNode atype kids)    = (AbstractNode atype kids)
easyCapChanges (Heredoc doc start end kids) = (Heredoc doc start end kids)


--noRemod gets rid of remods (since they don't exist in Perl 6)

noRemod :: [P5AST] -> [P5AST]
noRemod []   = []
noRemod kids = if (matchOnType (head kids) (LiteralNode "remod" "" "")) then (tail kids) else ((head kids):(noRemod (tail kids))) 


--This function applies changeSInternals in the proper places to translate everything inside a regex from Perl 5 -> Perl 6

regexInternals :: P5AST -> P5AST
regexInternals (AbstractNode "op_subst" kids) = (AbstractNode "op_subst" (changeSInternals kids))
regexInternals (AbstractNode "op_match" kids) = (AbstractNode "op_match" (changeMInternals kids))
regexInternals (AbstractNode atype kids)      = (AbstractNode atype (map regexInternals kids))
regexInternals (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
regexInternals (Heredoc doc start end kids)   = (Heredoc doc start end kids)


--This function does the actual work of finding the regex and applying the change (for substitution regexs).

changeSInternals :: [P5AST] -> [P5AST]
changeSInternals []   = []
changeSInternals kids =  if ((length kids) >= 3) then if (and [((head ((extractUni (head kids))++" "))=='s'),(matchOnType (head kids) (LiteralNode "openquote" "" "")),(matchOnType (head (drop 1 kids)) (LiteralNode "text" "" "")),(matchOnType (head (drop 2 kids)) (LiteralNode "closequote" "" ""))]) then [(head kids),(LiteralNode "text" "1" (regexChangeCaptures (regexChange (extractUni (head (drop 1 kids)))))),(head (drop 2 kids))]++(drop 3 kids)
        else ((head kids):(changeSInternals (drop 1 kids)))
    else ((head kids):(changeSInternals (drop 1 kids)))


--This function does the actual work of finding the regex and applying the change (for match regexs).

changeMInternals :: [P5AST] -> [P5AST]
changeMInternals []   = []
changeMInternals kids = if (matchOnType (head kids) (LiteralNode "text" "" "")) then (LiteralNode "text" "" (regexChangeCaptures (regexChange (extractUni (head kids))))):(drop 1 kids) else (head kids):(changeMInternals (drop 1 kids)) 


--Function to apply the regexString parser to a string.
--If the parser fails, it terminates quietly, returning the input string and printing an error message

regexChange :: String -> String
regexChange instr = case parse regexString "regex" instr of
    Left err -> do{[putStrLn ("\nNon-Terminal Error:\n" ++ show err ++"\nIN:\n"++instr)]; instr}
    Right result -> result


--Function to apply the captureString parser to a string, aliasing all captures so that $1 (and friends) are still the same in Perl 6.
--If the parser fails, it just returns the input and prints an error message

regexChangeCaptures :: String -> String
regexChangeCaptures instr = case parse (captureString 1) "regex (Captures)" instr of
    Left err -> do{[putStrLn ("\nNon-Terminal Error:\n" ++ show err ++"\nIN:\n"++instr)]; instr}
    Right result -> result


--Parses all captures from a regex into Perl 6 aliases.
--Each capture becomes a Perl 6 aliased capture in the Perl 5 order of captures. For the sake of simplicity and reliability, 
--all captures are explicitly aliased. 

captureString :: Int -> Parser String
captureString depth = do{ strs <- manyTill (choice[ do{try(string "\\("); return "\\("},
                                                    do{try(string "\\)"); return "\\)"},
                                                    do{try(string "(?"); return "(?"},
                                                    do{try(string "("); more <- (captureString (depth+1)); return (" $"++(show depth)++":=["++more)},
                                                    do{try(string ")"); return "] "},
                                                    do{char <- anyToken; return (char:"")}]) eof;
    return(joinString strs);}


--A regex string in Perl 5 may be null, but it can't be implicitly null in Perl 6.
--A completely null Perl 5 regex becomes a Perl 6 /<prior>/.
--If it's not null, it's a series of regex characters, handled by the regexChar parser

regexString :: Parser String
regexString = choice[ do{try(eof); return "<prior>"},
                      do{try(char '{'); strs <- (manyTill regexChar eof); return ('{':(joinString strs))},
                      do{strs <- (manyTill regexChar eof); return (joinString strs)}]


--The regexChar parser takes care of all changed metacharacters, as well as de-meta-ing
--the > and < characters (to \> and \<), quotestrings, classes of characters ([abc] and [^abc])
--(? operations, null alternatives and last but not least, counts. The countRegex parser is used
--for some help on counts.

regexChar :: Parser String
regexChar = choice[do{try(string "\\\\"); return "\\\\"},      --Get rid of literal \, make sure only metacharacter \ can trigger the other choices
                   do{try(string "\\("); return "\\("},        --Make sure we don't get a literal paren (this way only metcharacter parens can hit the other choices
                   do{try(string "\\)"); return "\\)"},        --Don't let "\)" get confused with ")"
                   do{try(string "\\{"); return "\\{"},        --Don't confuse "\{" and "{"
                   do{try(string "\\}"); return "\\}"},        --Don't confuse "\}" and "}"
                   do{try(string "\\]"); return "\\]"},        --Don't let "\]" take the place of "]"
                   do{try(string " "); return "<sp>"},         --Handle (space) -> <sp>
                   do{try(string "${"); return "${"},
                   do{try(string "\\$"); name <- many alphaNum; choice[do{char '['; key <- manyTill anyToken (char ']'); return ("@"++name++"["++key++"]")}, do{char '{'; key <- manyTill anyToken (char '}'); return ("%"++name++"<"++key++">")}, return ("$"++name)]},
                   do{try(string "\\A"); return "^"},          -- \A -> ^
                   do{try(string "\\z"); return "$"},          -- \z -> $
                   do{try(string "\\Z"); return "\\n?$"},      -- \Z -> \n?$
                   do{try(string "\\n"); return "\\c[LF]"},    -- \n -> \c[LF]
                   do{try(string "\\r?\\n"); return "\\n"},    -- \r?\n -> \n
                   do{try(string "[^\\n]"); return "\\C[LF]"}, -- [^\n] -> \C[LF]
                   do{try(string "\\a"); return "\\c[BEL]"},   -- \a -> \c[BEL]
                   do{try(string "\\N{"); val <- (manyTill anyToken (char '}')); return ("\\c["++val++"]")},                -- \N{CENT SIGN} -> \c[CENT SIGN]
                   do{try(string "[^\\N{"); val <- (manyTill anyToken (string "}]")); return ("\\C["++val++"]")},           -- [^\N{CENT SIGN}] -> \C[CENT SIGN]
                   do{try(string "\\c["); return "\\e"},       -- \c[ -> \e
                   do{try(string "[^\\t]"); return "\\T"},     -- [^\t] -> \T
                   do{try(string "[^\\r]"); return "\\R"},     -- [^\r] -> \R
                   do{try(string "[^\\f]"); return "\\F"},     -- [^\f] -> \F
                   do{try(string "[^\\e]"); return "\\E"},     -- [^\e] -> \E
                   do{try(string "\\x{"); val <- (manyTill anyToken (char '}')); return ("\\x["++val++"]")},                   -- \x{3b1} -> \x[3b1]
                   do{try(string "[^\\x"); val <- (manyTill anyToken (char ']')); return ("\\X"++val)},                      -- [^\x1B] -> \X1B
                   do{try(string "[^\\x{"); val <- (manyTill anyToken (string "}]")); return ("\\X["++val++"]")},            -- [^\x{263a}] -> \X[263a]
                   do{try(string "\\p{"); prop <- (manyTill anyToken (char '}')); return ("<prop "++prop++">")},             -- \p{prop} -> <prop>
                   do{try(string "\\P{"); prop <- (manyTill anyToken (char '}')); return ("<-prop "++prop++">")},            -- \P{prop} -> <-prop>
                   do{try(string "\\X"); return "<.>"},        -- \X -> <.>
                   do{try(string "<"); return "\\<"},          -- < -> \<
                   do{try(string ">"); return "\\>"},          -- > -> \>
                   do{try(string "\\Q"); str <- (manyTill regexChar (string "\\E")); return ("<{ quotemeta '"++(joinString str)++"' }>")}, -- \Qstring\E -> <{ quotemeta 'string'}>
                   do{num <- try(do{string "\\"; num <- digit; return num}); return ("$"++(num:""))},                        -- \1 -> $1 
                   do{try(string "[^"); atoms <- (manyTill regexChar (char ']')); return ("<-["++(joinString atoms)++"]>")}, -- [^abc] -> <-[abc]>
                   do{try(string "["); atoms <- (manyTill regexChar (char ']')); return ("<["++(joinString atoms)++"]>")},   -- [abc] -> <[abc]>
                   do{try(string "(?:"); atoms <- (manyTill regexChar (char ')')); return ("["++(joinString atoms)++"]")},                 -- (?:...) -> [...]
                   do{try(string "(?="); atoms <- (manyTill regexChar (char ')')); return ("<?before "++(joinString atoms)++">")},         -- (?=foo) -> <?before foo>
                   do{try(string "(?!"); atoms <- (manyTill regexChar (char ')')); return ("<!before "++(joinString atoms)++">")},         -- (?!foo) -> <!before foo>
                   do{try(string "(?<="); atoms <- (manyTill regexChar (char ')')); return ("<?after "++(joinString atoms)++">")},         -- (?<=foo) -> <?after foo>
                   do{try(string "(?<!"); atoms <- (manyTill regexChar (char ')')); return ("<!after "++(joinString atoms)++">")},         -- (?<!foo) -> <!after foo>
                   do{try(string "(?>"); atoms <- (manyTill regexChar (char ')')); return ("["++(joinString atoms)++"]:")},                -- (?>...) -> [...]:
                   do{try(string "{"); countMod <- (manyTill anyToken (char '}')); return (countRegex (countMod++"|"))},     -- x{2} -> x**{2} and x{2,} -> x**{2..} and x{2,3} -> x**{2..3}
                   do{try(string "(?{"); code <- (manyTill anyToken (string "})")); return ("{"++code++"}")},                -- (?{...}) -> {...}
                   do{try(string "(?("); cond <- (manyTill regexChar (char ')')); tru <- (manyTill regexChar (char '|')); els <- (manyTill regexChar (char ')')); return ("[ "++(joinString cond)++" :: "++(joinString tru)++" | "++(joinString els)++" ]")},
                   do{try(string "(??{"); code <- (manyTill anyToken (string "})")); return ("<{"++code++"}>")},             -- (??{...}) -> <{...}>
                   do{try(do{string "|"; eof}); return "<null>"},                                                            -- /blah|/ -> /blah|<null>/
                   do{char <- anyToken; return (char:"")}]


--Wrapper for parsing the count modifier in regexs.
--If it fails, it moves along nicely and returns it's input while printing an error message.

countRegex :: String -> String
countRegex instr = case parse countString "regex count" instr of
    Left err -> do{[putStrLn ("\nNon-Terminal Error:\n" ++ show err ++"\nIN:\n"++instr)]; (reverse (tail (reverse instr)))}
    Right result -> result


--Parser that translates regex counts from Perl 5 to Perl 6. Takes care of x{2} -> x**{2} and x{2,} -> x**{2..} and x{2,3} -> x**{2..3},
--as well as all of those with '?' at the end (the '?' is just left untouched).

countString :: Parser String
countString = do
    firstNum <- manyTill digit (choice[char ',', char '|'])
    spaces
    theRest <- choice[do{try(eof); return ""},
                      do{try(char '|'); return "..."},
                      do{secondNum <- manyTill digit (char '|'); return (".."++secondNum)}]
    return ("**{"++firstNum++theRest++"}")


--When the split is implicitly on the topic ($_), it can become a method call (.split(...))

topicSplit :: P5AST -> P5AST
topicSplit (AbstractNode "op_split" kids) = (AbstractNode "op_split" (topicMethod kids))
topicSplit (AbstractNode atype kids)      = (AbstractNode atype (map topicSplit kids))
topicSplit (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
topicSplit (Heredoc doc start end kids)   = (Heredoc doc start end (map topicSplit kids))


--Actually convert to a method.

topicMethod :: [P5AST] -> [P5AST]
topicMethod []   = []
topicMethod kids = if (not (isIn (AbstractNode "listelem" []) kids)) then [(LiteralNode "junk" "" "")] else kids


--translates foreach loops:
--foreach $foo (@bar) {...} becomes foreach @bar -> $foo {...}

foreachTranslation :: P5AST -> P5AST
foreachTranslation (AbstractNode "op_leaveloop" kids) = if (isIn (LiteralNode "token" "1" "foreach") kids) then (AbstractNode "op_leaveloop" (newForeach kids)) else (AbstractNode "op_leaveloop" (map foreachTranslation kids))                                             
foreachTranslation (AbstractNode atype kids)          = (AbstractNode atype (map foreachTranslation kids))
foreachTranslation (LiteralNode atype enc uni)        = (LiteralNode atype enc uni)
foreachTranslation (Heredoc doc start end kids)       = (Heredoc doc start end kids)


--Does the dirty work of actual translation on foreach

newForeach :: [P5AST] -> [P5AST]
newForeach [] = []
newForeach kids = if (matchOnType (head kids) (LiteralNode "junk" "" "")) then (head kids):(newForeach (drop 1 kids)) else 
    case [(isIn (AbstractNode "op_padsv" []) kids), (isIn (AbstractNode "op_padav" []) kids), (isIn (AbstractNode "op_list" []) kids), (isIn (AbstractNode "op_rv2av" []) kids), (isIn (AbstractNode "op_gv" []) kids), (isIn (LiteralNode "declarator" "" "my") kids), (isIn (AbstractNode "op_rv2gv" []) kids)] of
        [True, True, False, False, False, False, False]  -> (map foreachTranslation [(LiteralNode "token" "1" "foreach"),(LiteralNode "junk" "1" " "),(extractNodetype (AbstractNode "op_padav" []) kids),(AbstractNode "op_iter" []),(LiteralNode "junk" "1" " "),(LiteralNode "operator" "1" "->"),(LiteralNode "junk" "1" " "),(LiteralNode "declarator" "1" "my"),(extractNodetype (AbstractNode "op_padsv" []) kids),(extractNodetype (AbstractNode "op_lineseq" []) kids)])
        [True, False, True, False, False, True, False]  -> (map foreachTranslation [(LiteralNode "token" "1" "foreach"),(LiteralNode "junk" "1" " "),(LiteralNode "opener" "1" "("),(extractNodetype (AbstractNode "op_list" []) kids),(LiteralNode "closer" "1" ")"),(AbstractNode "op_iter" []),(LiteralNode "junk" "1" " "),(LiteralNode "operator" "1" "->"),(LiteralNode "junk" "1" " "),(LiteralNode "declarator" "1" "my"),(extractNodetype (AbstractNode "op_padsv" []) kids),(extractNodetype (AbstractNode "op_lineseq" []) kids)])
        [False, False, False, True, False, False, False] -> (map foreachTranslation [(LiteralNode "token" "1" "foreach"),(LiteralNode "junk" "1" " "),(extractNodetype (AbstractNode "op_rv2av" []) kids),(AbstractNode "op_iter" []),(LiteralNode "junk" "1" " "),(LiteralNode "operator" "1" "->"),(LiteralNode "junk" "1" " "),(extractNodetype (AbstractNode "op_rv2gv" []) kids),(extractNodetype (AbstractNode "op_lineseq" []) kids)])
        [False, False, True, False, False, False, True] -> (map foreachTranslation [(LiteralNode "token" "1" "foreach"),(LiteralNode "junk" "1" " "),(LiteralNode "opener" "" "("),(extractNodetype (AbstractNode "op_list" []) kids),(LiteralNode "closer" "" ")"),(AbstractNode "op_iter" []),(LiteralNode "junk" "1" " "),(LiteralNode "operator" "1" "->"),(LiteralNode "junk" "1" " "),(extractNodetype (AbstractNode "op_rv2gv" []) kids),(extractNodetype (AbstractNode "op_lineseq" []) kids)])
        [False, False, True, False, False, False, False] -> (map foreachTranslation [(LiteralNode "token" "1" "foreach"),(LiteralNode "junk" "1" " "),(extractNodetype (AbstractNode "op_rv2av" []) kids),(AbstractNode "op_iter" []),(LiteralNode "junk" "1" " "),(LiteralNode "operator" "1" "->"),(LiteralNode "junk" "1" " "),(LiteralNode "sigil" "" "$_"),(extractNodetype (AbstractNode "op_lineseq" []) kids)])
        [False, False, True, False, True, False, False] -> (map foreachTranslation [(LiteralNode "token" "1" "foreach"),(LiteralNode "junk" "1" " "),(LiteralNode "opener" "1" "("),(extractNodetype (AbstractNode "op_list" []) kids),(LiteralNode "closer" "1" ")"),(AbstractNode "op_iter" []),(LiteralNode "junk" "1" " "),(LiteralNode "operator" "1" "->"),(LiteralNode "junk" "1" " "),(LiteralNode "sigil" "" "$_"),(extractNodetype (AbstractNode "op_lineseq" []) kids)])
        _                           -> []


--Changes a split on a sinle space into a call to the .words method

toWords :: P5AST -> P5AST
toWords (AbstractNode "op_split" kids) = if (and [(isIn (AbstractNode "op_const" []) kids),(isInSequence [(LiteralNode "openquote" "1" "'"), (LiteralNode "text" "1" " "), (LiteralNode "closequote" "1" "'")] (extractKids (extractNodetype (AbstractNode "op_const" []) kids)))]) then (AbstractNode "op_split" [(getSecondArg kids), (LiteralNode "operator" "1" "."), (AbstractNode "op_method" [(AbstractNode "op_const" [(LiteralNode "token" "1" "words")])])])
    else (AbstractNode "op_split" (map toWords kids))
toWords (AbstractNode atype kids)      = (AbstractNode atype (map toWords kids))
toWords (Heredoc doc start end kids)   = (Heredoc doc start end kids)
toWords (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)


--Changes close($fh) to $fh.close
--Only applied when -Oo

closeToMethod :: P5AST -> P5AST
closeToMethod (AbstractNode "op_close" kids) = (AbstractNode "op_close" (changeCloseMethod kids))
closeToMethod (AbstractNode atype kids)      = (AbstractNode atype (map closeToMethod kids))
closeToMethod (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
closeToMethod (Heredoc doc start end kids)   = (Heredoc doc start end kids)

--This function actually adds the close method

changeCloseMethod :: [P5AST] -> [P5AST]
changeCloseMethod []   = []
changeCloseMethod kids = case (matchWithoutEnc (head kids) (LiteralNode "operator" "" "close")) of
    True  -> [(head (tail kids)), (LiteralNode "operator" "1" "."), (AbstractNode "op_method" [(AbstractNode "op_const" [(LiteralNode "token" "1" "close")])])]++(drop 2 kids)
    False -> (head kids):(changeCloseMethod (drop 1 kids))


--Changes the quote type of regexs in a split.

splitQuotes :: P5AST -> P5AST
splitQuotes (AbstractNode "op_split" kids) = (AbstractNode "op_split" (join (map toSlashQuotes kids)))
splitQuotes (AbstractNode atype kids)      = (AbstractNode atype (map splitQuotes kids))
splitQuotes (Heredoc doc start end kids)   = (Heredoc doc start end kids)
splitQuotes (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)


--Changes single quotes to slashes.

toSlashQuotes :: P5AST -> [P5AST]
toSlashQuotes (AbstractNode "op_const" kids) = [(LiteralNode "openquote" "1" "/"), (extractText kids),(LiteralNode "closequote" "1" "/")]
toSlashQuotes (Heredoc doc start end kids)   = [(Heredoc doc start end kids)]
toSlashQuotes (LiteralNode atype enc uni)    = [(LiteralNode atype enc uni)]
toSlashQuotes (AbstractNode atype kids)      = [(AbstractNode atype kids)]


--Changes length($blah) to $blah.chars with the help of toCharMethod

lengthToMethod :: P5AST -> P5AST
lengthToMethod (AbstractNode "op_length" kids) = (AbstractNode "op_length" (toCharMethod kids))
lengthToMethod (AbstractNode atype kids)       = (AbstractNode atype (map lengthToMethod kids))
lengthToMethod (Heredoc doc start end kids)    = (Heredoc doc start end kids)
lengthToMethod (LiteralNode atype enc uni)     = (LiteralNode atype enc uni)


--Actually performs change to method call

toCharMethod :: [P5AST] -> [P5AST]
toCharMethod [] = []
toCharMethod kids = if ((length kids) >= 2) then case [(matchWithoutEnc (head kids) (LiteralNode "operator" "" "length")), (matchWithoutEnc (head (drop 1 kids)) (LiteralNode "opener" "" "("))] of
        [True, True]   -> [(head (drop 2 kids)), (LiteralNode "operator" "" "."), (AbstractNode "op_method" [(AbstractNode "op_const" [(LiteralNode "token" "1" "chars")])])]++(drop 4 kids)
        [True, False]  -> [(head (drop 1 kids)),(LiteralNode "operator" "" "."), (AbstractNode "op_method" [(AbstractNode "op_const" [(LiteralNode "token" "1" "chars")])])]++(drop 2 kids)
        [False, True]  -> (head kids):(toCharMethod (drop 1 kids))
        [False, False] -> (head kids):(toCharMethod (drop 1 kids))
    else kids


{-Translates split calls on a regex with an explicit match (i.e. split(/blah/m, $something) to no longer
use the /m which now happens immediately. -}

splitOnMatchTranslate :: P5AST -> P5AST
splitOnMatchTranslate (AbstractNode "op_split" kids) = (AbstractNode "op_split" (map removeMModifier kids))
splitOnMatchTranslate (AbstractNode atype kids)      = (AbstractNode atype (map splitOnMatchTranslate kids))
splitOnMatchTranslate (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
splitOnMatchTranslate (Heredoc doc start end kids)   = (Heredoc doc start end kids)


{-Removes the m modifier from a regex-}

removeMModifier :: P5AST -> P5AST
removeMModifier (LiteralNode "openquote" enc "m/") = (LiteralNode "openquote" enc "/")
removeMModifier (LiteralNode atype enc uni)        = (LiteralNode atype enc uni)
removeMModifier (AbstractNode atype kids)          = (AbstractNode atype kids)
removeMModifier (Heredoc doc start end kids)       = (Heredoc doc start end kids)


--This function does all the work of tranlsating <FH> to $fh.readline.
--When an Op_readline is found, it's replaced with a new Op_readling consisting
--of a .readline method call from the filehandle that was being read.

readlineTranslate :: P5AST -> P5AST
readlineTranslate (AbstractNode "op_readline" kids) = (AbstractNode "op_readline" (readlineMethod kids))
readlineTranslate (AbstractNode atype kids)         = (AbstractNode atype (map readlineTranslate kids))
readlineTranslate (LiteralNode atype enc uni)       = (LiteralNode atype enc uni)
readlineTranslate (Heredoc doc start end kids)      = (Heredoc doc start end kids)

{-Actually adds a method call to .readline to a filehandle-}
readlineMethod :: [P5AST] -> [P5AST]
readlineMethod []   = []
readlineMethod kids = case (matchOnType (head kids) (LiteralNode "token" "" "")) of
    True  -> if ((length kids) >= 2) then case ((head (tail (extractUni (head (tail kids)))))=='$') of 
            True  -> [(LiteralNode "sigil" "" (drop 1 (reverse (drop 1 (reverse (extractUni (head kids))))))), (LiteralNode "operator" "1" "."), (AbstractNode "op_method" [(AbstractNode "op_const" [(LiteralNode "token" "1" "readline")])])]++(drop 1 kids)
            False -> [(LiteralNode "sigil" "" ('$':(drop 1 (reverse (drop 1 (reverse (extractUni (head kids)))))))), (LiteralNode "operator" "1" "."), (AbstractNode "op_method" [(AbstractNode "op_const" [(LiteralNode "token" "1" "readline")])])]++(drop 1 kids)
        else kids
    False -> (head kids):(readlineMethod (drop 1 kids))

{-Move regex mods from the end of a regex to the beginning, using the new :i, :g, etc sugar. -}

regexModifiers :: P5AST -> P5AST
regexModifiers (AbstractNode "op_subst" kids) = case [(matchOnType (LiteralNode "remod" "" "") (extractNodetype (LiteralNode "remod" "" "") kids)), ('g' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids))), ('e' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids)))] of
    [True, True, True, True]    -> (AbstractNode "op_subst" (dropEMod (moveGMod (moveIMod kids))))
    [True, True, True, False]   -> (AbstractNode "op_subst" (moveGMod (moveIMod kids)))
    [True, True, False, True]   -> (AbstractNode "op_subst" (moveGMod (dropEMod kids)))
    [True, False, True, True]   -> (AbstractNode "op_subst" (moveIMod (dropEMod kids)))
    [True, True, False, False]  -> (AbstractNode "op_subst" (moveGMod kids))
    [True, False, False, True]  -> (AbstractNode "op_subst" (dropEMod kids))
    [True, False, True, False]  -> (AbstractNode "op_subst" (moveIMod kids))
    [True, False, False, False] -> (AbstractNode "op_subst" kids)
    _                           -> (AbstractNode "op_subst" kids)
regexModifiers (AbstractNode "op_match" kids) = case [(matchOnType (LiteralNode "remod" "" "") (extractNodetype (LiteralNode "remod" "" "") kids)), ('g' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids)))] of
    [True, True, True]    -> (AbstractNode "op_subst" (moveIMod (moveGMod kids)))
    [True, True, False]   -> (AbstractNode "op_subst" (moveIMod kids))
    [True, False, True]   -> (AbstractNode "op_subst" (moveGMod kids))
    _                           -> (AbstractNode "op_subst" kids)
regexModifiers (AbstractNode "op_split" kids) = case [(matchOnType (LiteralNode "remod" "" "") (extractNodetype (LiteralNode "remod" "" "") kids)), ('g' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode "remod" "" "") kids)))] of
    [True, True, True]    -> (AbstractNode "op_subst" (moveIMod (moveGMod kids)))
    [True, True, False]   -> (AbstractNode "op_subst" (moveIMod kids))
    [True, False, True]   -> (AbstractNode "op_subst" (moveGMod kids))
    _                           -> (AbstractNode "op_subst" kids)
regexModifiers (AbstractNode atype kids)      = (AbstractNode atype (map regexModifiers kids))
regexModifiers (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
regexModifiers (Heredoc doc start end kids)   = (Heredoc doc start end kids)


{-Moves an i modifier from the end to a :i at the beginning. -}

moveIMod :: [P5AST] -> [P5AST]
moveIMod []   = []
moveIMod kids = case [(matchOnType (head kids) (LiteralNode "openquote" "" "")), (matchOnType (head kids) (LiteralNode "remod" "" ""))] of
    [True, False]  -> (LiteralNode "openquote" "" ((head (reverse (extractUni (head kids)))):"i:"++(tail (reverse (extractUni (head kids))))) ):(drop 1 kids)
    [False, True]  -> (LiteralNode "remod" "" (removeChar 'i' (extractUni (head kids)))):(drop 1 kids)
    [False, False] -> (head kids):(moveIMod (tail kids))

{-Moves a g modifier from the end to a :g at the beginning. -}

moveGMod :: [P5AST] -> [P5AST]
moveGMod []   = []
moveGMod kids = case [(matchOnType (head kids) (LiteralNode "openquote" "" "")), (matchOnType (head kids) (LiteralNode "remod" "" ""))] of
    [True, False]  -> (LiteralNode "openquote" "" (reverse ((head (reverse (extractUni (head kids)))):"g:"++(tail (reverse (extractUni (head kids)))))) ):(moveGMod (drop 1 kids))
    [False, True]  -> (LiteralNode "remod" "" (removeChar 'g' (extractUni (head kids)))):(drop 1 kids)
    [False, False] -> (head kids):(moveGMod (tail kids))

{-Completely drops the e mod, since it's now not needed. -}

dropEMod :: [P5AST] -> [P5AST]
dropEMod []   = []
dropEMod kids = case [(matchOnType (head kids) (LiteralNode "text" "" "")), (matchOnType (head kids) (LiteralNode "remod" "" ""))] of
    [True, False] -> (head kids):(head (drop 1 kids)):(LiteralNode "opener" "" "{ "):(head (drop 2 kids)):(LiteralNode "opener" "" " }"):(dropEMod (drop 3 kids))
    [False, True] -> (LiteralNode "remod" "" (removeChar 'e' (extractUni (head kids)))):(drop 1 kids)
    _             -> (head kids):(dropEMod (tail kids))


{-Translates =~ -> ~~ for using regexs in P6
The name of the function is a bit long, but it won't be called often and at least it's very descriptive -}

equalTildeToTildeTilde :: P5AST -> P5AST
equalTildeToTildeTilde (LiteralNode "operator" enc "=~") = (LiteralNode "operator" enc "~~")
equalTildeToTildeTilde (AbstractNode atype kids)         = (AbstractNode atype (map equalTildeToTildeTilde kids))
equalTildeToTildeTilde (LiteralNode atype enc uni)       = (LiteralNode atype enc uni)
equalTildeToTildeTilde (Heredoc doc start end kids)      = (Heredoc doc start end kids)


{- Function that converts conditional return statements (i.e. "a ? b : c") into a P5 form
a ?? b !! c.  No Context needed, if ? or : is ever a P5 operator, it's in one of these statements-}

conditionalExpression :: P5AST -> P5AST
conditionalExpression (LiteralNode "punct" enc "?") = (LiteralNode "punct" enc "??")
conditionalExpression (LiteralNode "punct" enc ":") = (LiteralNode "punct" enc "!!")
conditionalExpression (LiteralNode atype enc uni)   = (LiteralNode atype enc uni)
conditionalExpression (AbstractNode atype kids)     = (AbstractNode atype (map conditionalExpression kids))
conditionalExpression (Heredoc doc start end kids)  = (Heredoc doc start end kids)


{-Changes to arrays with keys, namely $array[i] -> @array[i]-}

arrayKey :: P5AST -> P5AST
arrayKey (AbstractNode "op_aelem" kids) = if (isIn (AbstractNode "op_rv2av" []) kids) then (AbstractNode "op_aelem" (map arrayKeyChanges kids))
    else (AbstractNode "op_aelem" (map arrayKey kids))
arrayKey (LiteralNode "sigil" enc uni) = if ('[' `elem` uni) then (scalarSigilToArraySigil (LiteralNode "sigil" enc uni)) else (LiteralNode "sigil" enc uni)
arrayKey (AbstractNode atype kids)     = (AbstractNode atype (map arrayKey kids))
arrayKey (LiteralNode atype enc uni)   = (LiteralNode atype enc uni)
arrayKey (Heredoc doc start end kids)  = (Heredoc doc start end (map arrayKey kids))


{-Actually applies the changes needed for the arrayKey function-}

arrayKeyChanges :: P5AST -> P5AST
arrayKeyChanges (AbstractNode "op_rv2av" kids) = (AbstractNode "op_rv2av" (map scalarSigilToArraySigil kids))
arrayKeyChanges (LiteralNode "sigil" enc uni)  = (scalarSigilToArraySigil (LiteralNode "sigil" enc uni))
arrayKeyChanges (AbstractNode atype kids)      = (AbstractNode atype kids)
arrayKeyChanges (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
arrayKeyChanges (Heredoc doc start end kids)   = (Heredoc doc start end kids)


{-$something->@something, used by the arrayKeyChanges function-}

scalarSigilToArraySigil :: P5AST -> P5AST
scalarSigilToArraySigil (LiteralNode "sigil" enc uni) = (LiteralNode "sigil" enc ('@':(drop 1 uni)))
scalarSigilToArraySigil (LiteralNode atype enc uni)   = (LiteralNode atype enc uni)
scalarSigilToArraySigil (AbstractNode atype kids)     = (AbstractNode atype kids)
scalarSigilToArraySigil (Heredoc doc start end kids)  = (Heredoc doc start end kids)


{-Do changes to hashes with keys, such as $hash{word}->%hash<word> and $hash{$var}->%hash{$var}-}

hashKey :: P5AST -> P5AST
hashKey (AbstractNode "op_helem" kids) = if (isInOrder [(AbstractNode "op_rv2hv" []), (LiteralNode "opener" "1" "{"), (AbstractNode "op_const" []), (LiteralNode "closer" "1" "}")] kids) 
    then (AbstractNode "op_helem" (map constHashChanges kids)) else
        if (isInOrder [(AbstractNode "op_rv2hv" []), (LiteralNode "opener" "1" "{"), (LiteralNode "closer" "1" "}")] kids)
             then (AbstractNode "op_helem" (map hashChanges kids)) 
                else (AbstractNode "op_helem" (map hashKey kids)) 
hashKey (LiteralNode "sigil" enc uni)  = case [('{' `elem` uni), (and [((head uni)=='$'), ((head (tail uni))=='{'), ((head (drop 2 uni))=='^')])] of
    [True, True]  -> (LiteralNode "sigil" enc ("$*"++(drop 3 (reverse (tail (reverse uni))))))
    [True, False] -> (LiteralNode "sigil" enc ('%':(tail uni)))
    _                 -> (LiteralNode "sigil" enc uni)
hashKey (AbstractNode atype kids)      = (AbstractNode atype (map hashKey kids))
hashKey (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
hashKey (Heredoc doc start end kids)   = (Heredoc doc start end (map hashKey kids))


{-Actually applie changes for hashKey-}

hashChanges :: P5AST -> P5AST
hashChanges (AbstractNode "op_rv2hv" kids) = (AbstractNode "op_rv2hv" (map scalarSigilToHashSigil kids))
hashChanges (AbstractNode atype kids)      = (AbstractNode atype kids)
hashChanges (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
hashChanges (Heredoc doc start end kids)   = (Heredoc doc start end kids)


{-Additional changes for when a has has a constant key ({word}-><word>)-}

constHashChanges :: P5AST -> P5AST
constHashChanges (LiteralNode "opener" enc "{") = (LiteralNode "opener" enc "<")
constHashChanges (LiteralNode "closer" enc "}") = (LiteralNode "closer" enc ">")
constHashChanges (AbstractNode "op_rv2hv" kids) = (AbstractNode "op_rv2hv" (map scalarSigilToHashSigil kids))
constHashChanges (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)
constHashChanges (AbstractNode atype kids)      = (AbstractNode atype kids)
constHashChanges (Heredoc doc start end kids)   = (Heredoc doc start end kids)


{-Function to change the sigil from a scalar ($something) to a hash (%something)-}

scalarSigilToHashSigil :: P5AST -> P5AST
scalarSigilToHashSigil (LiteralNode "sigil" enc uni) = (LiteralNode "sigil" enc ('%':(drop 1 uni)))
scalarSigilToHashSigil (LiteralNode atype enc uni)   = (LiteralNode atype enc uni)
scalarSigilToHashSigil (AbstractNode atype kids)     = (AbstractNode atype kids)


{-
A main function to parse a file containing a tree and output the contents to another file
Useage: mainParse inFile outFile options
options is a string with optional information, created by the getModifiers function. mainParse only cares
if there's a 'v' in the options string, which starts verbose mode.
-}

mainParse :: FilePath -> FilePath -> String -> IO ()
mainParse inName outName options= do
    --outHandle   <- openFile outName WriteMode
    putStrLn "--Perl 5 to Perl 6 Translation Utility--"
    putStrLn ("Translating: "++inName)
    putStrLn ("Output Target: "++outName)
    putStrLn ("Parsing Input File...")
    result <- parseFromFile parseInput inName
    let ast = case (result) of
            Left err -> error $ "\nError:\n" ++ show err
            Right result -> result
    --putStrLn (unlines (getUndeclaredVars (AbstractNode "P5AST" ast)))
    if ('n' `elem` options) then (putStrLn "Not Translating...")  else case [('o' `elem` options), ('r' `elem` options), ('s' `elem` options)] of 
        [True, True, False]   -> putStrLn ("Translating with the heavy object oriented option and limited regex support...")
        [True, False, False]  -> putStrLn ("Translating with the heavy object oriented option...")
        [False, True, False]  -> putStrLn ("Translating with limited regex support...")
        [False, False, False] -> putStrLn ("Translating...")
        [True, True, True]    -> putStrLn ("Translating with the heavy object oriented option and limited regex support and no strict...")
        [True, False, True]   -> putStrLn ("Translating with the heavy object oriented option and no strict...")
        [False, True, True]   -> putStrLn ("Translating with limited regex support and no strict...")
        [False, False, True]  -> putStrLn ("Translating with no strict...")
    let tree = (translate (AbstractNode "P5AST" ast) options)
    outHandle   <- openFile outName WriteMode
    case [('v' `elem` options),('u' `elem` options)] of
        [True, True]   -> putStrLn "Printing with Verbose and Unknown Debug..."
        [False, True]  -> putStrLn "Printing with Unknown Debug..."
        [True, False]  -> putStrLn "Printing with Verbose..."
        [False, False] -> putStrLn "Printing..."
    case ('v' `elem` options) of
        True  -> do{putStrLn "VERBOSE: TREE(Translated):"; print tree; printTree outHandle tree options}
        False -> do{printTree outHandle tree options}
    hClose outHandle
    putStrLn "Finished."


--The actual main function. Just grabs the args and parses them into a mainParse call.

main = do 
    inargs <- getArgs
    mainParse (getFirstFile inargs) (getSecondFile inargs) (getModifiers inargs)


--The following functions parse out the command line args into a good format for the mainParse call.

--getModifiers creates a string with an element for each command line switch.
--Unknown swicthes (or file names) do nothing.

getModifiers :: [String] -> String
getModifiers []   = " "
getModifiers args = case (head args) of
    "-Oo"    ->  ('o':(getModifiers (drop 1 args)))
    "-V"     ->  ('v':(getModifiers (drop 1 args)))
    "-U"     ->  ('u':(getModifiers (drop 1 args)))
    "-R"     ->  ('r':(getModifiers (drop 1 args)))
    "-N"     ->  ('n':(getModifiers (drop 1 args)))
    "-S"     ->  ('s':(getModifiers (drop 1 args)))
    _        ->  (' ':(getModifiers (drop 1 args)))


--getFirstFile (oddly enough) gets the first file (which will be the second to last argument). 

getFirstFile :: [String] -> String
getFirstFile []   = ""
getFirstFile args = (head (drop 1 (reverse args))) 


--getSecondFile gets the second filename from the args. It should always be the last arument.

getSecondFile :: [String] -> String
getSecondFile []   = ""
getSecondFile args = (head (reverse args))
