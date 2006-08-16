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
translate tree options = if ('n' `elem` options) then (tree) else case [('o' `elem` options), ('r' `elem` options)] of
                               [True, False]   ->  (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (regexModifiers (regexOnce (scalarTranslate (hereDocTranslate (regexInternals (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree))))))))))))))))))))
                               [True, True]  ->  (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (easyRegex (changeVarsInQuotes  (scalarTranslate (regexOnce (hereDocTranslate (foreachTranslation (closeToMethod (lengthToMethod (splitOnMatchTranslate ({-splitQuotes-}(readlineTranslate (toWords (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree)))))))))))))))))))
                               [False, False]  -> (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (regexModifiers (regexOnce (scalarTranslate (hereDocTranslate (regexInternals (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree)))))))))))))))))
                               [False, True] -> (filePrintChange (changeExportOkay (getExportOkay tree) (changeExports (getExports tree) (changeVarsInQuotes (easyRegex (regexOnce (scalarTranslate (hereDocTranslate (foreachTranslation (splitOnMatchTranslate (splitQuotes (readlineTranslate (conditionalExpression (arrayKey (hashKey (equalTildeToTildeTilde tree))))))))))))))))

--Find any instance of print or printf that prints to a file and add a colon
filePrintChange :: P5AST -> P5AST
filePrintChange (AbstractNode Op_print kids) = if (or [(isInSequence kids [(LiteralNode Operator "" "print"),(AbstractNode Op_rv2gv [])]), (isInSequence kids [(LiteralNode Operator "" "print"),(LiteralNode Opener "" "("),(AbstractNode Op_rv2gv [])])]) then (AbstractNode Op_print (addPrintColon kids)) else (AbstractNode Op_print kids)
filePrintChange (AbstractNode Op_prtf kids) = if (or [(isInSequence kids [(LiteralNode Operator "" "printf"),(AbstractNode Op_rv2gv [])]), (isInSequence kids [(LiteralNode Operator "" "printf"),(LiteralNode Opener "" "("),(AbstractNode Op_rv2gv [])])]) then (AbstractNode Op_print (addPrintColon kids)) else (AbstractNode Op_print kids)
filePrintChange (AbstractNode atype kids) = (AbstractNode atype (map filePrintChange kids))
filePrintChange (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
filePrintChange (Heredoc start end kids) = (Heredoc start end kids)

--Adds a colon after the filehandle argument of print or printf
addPrintColon :: [P5AST] -> [P5AST]
addPrintColon [] =[]
addPrintColon kids = if (matchOnType (head kids) (AbstractNode Op_rv2gv [])) then (head kids):(LiteralNode Token "" ":"):(tail kids) else (head kids):(addPrintColon (tail kids))

--Finds sub declerations that match the names of things being exported and adds "is export" to them
changeExportOkay :: [String] -> P5AST -> P5AST
changeExportOkay [] (AbstractNode Sub kids) = (AbstractNode Sub kids)
changeExportOkay names (AbstractNode Sub kids) = if (isIn (LiteralNode Token "" (head names)) (extractKids (extractNodetype (AbstractNode Op_const []) kids))) then (AbstractNode Sub (addExportOkay kids)) else (changeExportOkay (tail names) (AbstractNode Sub kids))
changeExportOkay names (AbstractNode atype kids) = (AbstractNode atype (map (changeExportOkay names) kids))
changeExportOkay _ (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
changeExportOkay _ (Heredoc start end kids) = (Heredoc start end kids)

--Adds "is export" to anything that is @EXPORT_OK
addExportOkay :: [P5AST] -> [P5AST]
addExportOkay [] = []
addExportOkay kids = if (matchWithoutEnc (LiteralNode Opener "" "{") (head kids)) then (LiteralNode Token "" " is export"):(kids) else (head kids):(addExportOkay (tail kids))


--getExportOkay returns the names of everything assigned to @EXPORT_OK in a list of strings
getExportOkay :: P5AST -> [String]
getExportOkay (AbstractNode Op_aassign kids) = case [(matchOnType (smartHead kids) (AbstractNode Op_list [])), (matchWithoutEnc (smartHead (extractKids (smartHead (extractKids (smartHead kids))))) (LiteralNode Sigil "" "@EXPORT_OK")), (isIn (LiteralNode Operator "" "=") kids), (matchOnType (AbstractNode Quote []) (smartHead (extractKids (extractNodetype (AbstractNode Op_list []) (tail kids)))))] of
                                              [True, True, True, True] -> (extractExportsFromQW (extractKids (smartHead (extractKids (extractNodetype (AbstractNode Op_list []) (tail kids))))))
                                              _                        -> []
getExportOkay (AbstractNode atype kids) = (makeList (map getExportOkay kids))
getExportOkay (LiteralNode atype enc uni) = []
getExportOkay (Heredoc start end kids) = []

{-Changes things stored in @EXPORT to sub ... is export(:MANDATORY)-}
changeExports :: [String] -> P5AST -> P5AST
changeExports [] (AbstractNode Sub kids) = (AbstractNode Sub kids)
changeExports names (AbstractNode Sub kids) = if (isIn (LiteralNode Token "" (head names)) (extractKids (extractNodetype (AbstractNode Op_const []) kids))) then (AbstractNode Sub (addExport kids)) else (changeExports (tail names) (AbstractNode Sub kids))
changeExports names (AbstractNode atype kids) = (AbstractNode atype (map (changeExports names) kids))
changeExports _ (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
changeExports _ (Heredoc start end kids) = (Heredoc start end kids)

{-Actually adds the "is export(:MANDATORY)" text. Since there isn't really a way to add that to a P5AST, it's just stuck in a Token node.-}
addExport :: [P5AST] -> [P5AST]
addExport [] = []
addExport kids = if (matchWithoutEnc (LiteralNode Opener "" "{") (head kids)) then (LiteralNode Token "" " is export(:MANDATORY)"):(kids) else (head kids):(addExport (tail kids))

{-This function finds any lines that assign to @EXPORT and gets the names stored in @EXPORT so that they can be handed to changeExports. The captured names are returned in a list of strings (with the & stripped off for functions).-}
getExports :: P5AST -> [String]
getExports (AbstractNode Op_aassign kids) = case [(matchOnType (smartHead kids) (AbstractNode Op_list [])), (matchWithoutEnc (smartHead (extractKids (smartHead (extractKids (smartHead kids))))) (LiteralNode Sigil "" "@EXPORT")), (isIn (LiteralNode Operator "" "=") kids), (matchOnType (AbstractNode Quote []) (smartHead (extractKids (extractNodetype (AbstractNode Op_list []) (tail kids)))))] of
                                              [True, True, True, True] -> (extractExportsFromQW (extractKids (smartHead (extractKids (extractNodetype (AbstractNode Op_list []) (tail kids))))))
                                              _                        -> []
getExports (AbstractNode atype kids) = (makeList (map getExports kids))
getExports (LiteralNode atype enc uni) = []
getExports (Heredoc start end kids) = []

--My own version of head that doesn't fail on an empty list
smartHead :: [P5AST] -> P5AST
smartHead [] = (AbstractNode UnknownAbs [])
smartHead alist = head alist

--Makes a list of strings from a list of lists of strings
makeList :: [[String]] -> [String]
makeList [] = []
makeList alist = (head alist)++(makeList (tail alist))

--Grabs exports from something of the form @EXPORT = qw{blah foo bar}
extractExportsFromQW :: [P5AST] -> [String]
extractExportsFromQW [] = []
extractExportsFromQW kids = if (matchOnType (LiteralNode Openquote "" "") (head kids)) then (map (dropLeadingChar '&') (makeWords (extractUni (head (drop 1 kids))) [""])) else (extractExportsFromQW (tail kids))


--Take a string and return a list of strings with each element being a word in that string. 
--The second argument is what's already been collected (allowing you to start the list of words with your own list, if need be)
--To just get the contents of the string broken into words, just call makeWords instring [""]
makeWords :: String -> [String] -> [String]
makeWords [] curout = curout
makeWords inst curout= if ((head inst)==' ') then (makeWords (drop 2 inst) ([(head (tail inst))]:curout)) else (makeWords (tail inst) (((head curout)++[(head inst)]):(tail curout)))

--drop the leading character if it matches the first argument of the call
dropLeadingChar :: Char -> String -> String
dropLeadingChar _ [] = []
dropLeadingChar todrop astring = if ((head astring)==todrop) then (tail astring) else astring 


--Translates "@array" -> "@array[]" and "%hash" -> "%hash{}", as well as
--other changes needed inside quotes ($array[1] -> @array[1], etc.)
changeVarsInQuotes :: P5AST -> P5AST
changeVarsInQuotes (LiteralNode Text enc uni) = (LiteralNode Text enc (runTextParser uni))
changeVarsInQuotes (AbstractNode atype kids) = (AbstractNode atype (map changeVarsInQuotes kids))
changeVarsInQuotes (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
changeVarsInQuotes (Heredoc start end kids) = (Heredoc start end (map changeVarsInQuotes kids))

--Wrapper for the parser that translates arrays and hashes in strings
runTextParser :: String -> String
runTextParser instr = case parse textParser "text node" instr of
                                Left err -> error $ "\nError:\n" ++ show err
                                Right result -> result

--The actual parser that changes "@array" and "%hash"
textParser :: Parser String
textParser = do{ parts <- manyTill (choice[
                                           do{char '$'; name <- many alphaNum; choice[do{char '['; key <- manyTill anyToken (char ']'); return ("@"++name++"["++key++"]")}, do{char '{'; key <- manyTill anyToken (char '}'); return ("%"++name++"<"++key++">")}, return ("$"++name)]},
                                           do{char '@'; name <- many alphaNum; return ('@':name++"[]")},
                                           do{char '%'; name <- many alphaNum; return ('%':name++"{}")},
                                           do{this <- anyToken; return [this]}]) eof;
                 return (joinString parts)}

--Translates ?foo? to m:once/foo/ (and m?foo? to m:once/foo/)
--The order in which this is applied is very important, it should be applied
--BEFORE other regex modifiers (/i, for example) have been processed.
regexOnce :: P5AST -> P5AST
regexOnce (AbstractNode Op_match kids) = (AbstractNode Op_match (newOnce kids))
regexOnce (AbstractNode atype kids) = (AbstractNode atype (map regexOnce kids))
regexOnce (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
regexOnce (Heredoc start end kids) = (Heredoc start end kids)

--Changes the opening ? of a once regex to m:once/
newOnce :: [P5AST] -> [P5AST]
newOnce [] = []
newOnce kids = case [(matchWithoutEnc (head kids) (LiteralNode Openquote "" "m?")), (matchWithoutEnc (head kids) (LiteralNode Openquote "" "?"))] of
                 [True, False] -> (LiteralNode Openquote "" "m:once/"):(finalOnce (tail kids))
                 [False, True] -> (LiteralNode Openquote "" "m:once/"):(finalOnce (tail kids))
                 _             -> (head kids):(newOnce (tail kids))

--Changes the closing ? to / in a once regex
finalOnce :: [P5AST] -> [P5AST]
finalOnce [] = []
finalOnce kids = case [(matchOnType (LiteralNode Openquote "" "") (head kids)), ('?' `elem` (extractUni (head kids)))] of
                   [True, True] -> (LiteralNode Openquote "" ('/':(tail (extractUni (head kids))))):(tail kids)
                   _            -> (head kids):(finalOnce (tail kids))

--Find places where the translation scalar @blah -> +@blah is needed
scalarTranslate :: P5AST -> P5AST
scalarTranslate (AbstractNode Op_scalar kids) = (AbstractNode Op_scalar (scalarToPlus kids))
scalarTranslate (AbstractNode atype kids) = (AbstractNode atype (map scalarTranslate kids))
scalarTranslate (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
scalarTranslate (Heredoc start end kids) = (Heredoc start end kids)

--Change a scalar function call to + symbol.
scalarToPlus :: [P5AST] -> [P5AST]
scalarToPlus [] = []
scalarToPlus kids = case (matchWithoutEnc (head kids) (LiteralNode Operator "" "scalar")) of
                      False -> (head kids):(scalarToPlus (tail kids))
                      True  -> (LiteralNode Operator "" "+"):(dropLeadingJunk (head (drop 1 kids))):(drop 2 kids)

--Removes leading junk 
dropLeadingJunk :: P5AST -> P5AST
dropLeadingJunk (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
dropLeadingJunk (AbstractNode atype kids) = case (matchOnType (head kids) (LiteralNode Junk "" "")) of
                                              True  -> (AbstractNode atype (drop 1 kids))
                                              False -> (AbstractNode atype kids)
dropLeadingJunk (Heredoc start end kids) = (Heredoc start end kids)

--Translates herdocs. Just changes to start...
hereDocTranslate :: P5AST -> P5AST
hereDocTranslate (Heredoc start end kids) = (Heredoc (changeHereDoc start) end kids)
hereDocTranslate (AbstractNode atype kids) = (AbstractNode atype (map hereDocTranslate kids))
hereDocTranslate (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Change the syntax of heredocs
changeHereDoc :: P5AST -> P5AST
changeHereDoc (LiteralNode atype enc uni) = case ((head uni):(head (drop 1 uni)):[(head (drop 1 (drop 1 uni)))]) of
                                                    "<<\"" -> (LiteralNode atype enc ("qq:to/"++(drop 3 uni)++"/"))
                                                    "<<'"  -> (LiteralNode atype enc ("q:to/"++(drop 3 uni)++"/"))
                                                    _      -> (LiteralNode atype enc ("qq:to/"++(drop 2 uni)++"/"))

--easyRegex just moves around the remods and then changes the captures
easyRegex :: P5AST -> P5AST
easyRegex (AbstractNode Op_subst kids) = (AbstractNode Op_subst (easyRegexSChanges kids))
easyRegex (AbstractNode Op_match kids) = (AbstractNode Op_match (easyRegexMChanges kids))
easyRegex (AbstractNode atype kids) = (AbstractNode atype (map easyRegex kids))
easyRegex (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Just get the relevent Re mods and put them at the beginning
easyRegexSChanges :: [P5AST] -> [P5AST]
easyRegexSChanges [] = []
easyRegexSChanges kids = case [(matchOnType (head kids) (LiteralNode Openquote "" "")), ('g' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids)))] of
                          [True, True, True] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:i:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          [True, True, False] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          [True, False, True] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:i:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          [True, False, False] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:s"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          _ -> (head kids):(easyRegexSChanges (tail kids))

--Just get the relevent Re mods and put them at the beginning
easyRegexMChanges :: [P5AST] -> [P5AST]
easyRegexMChanges [] = []
easyRegexMChanges kids = case [(matchOnType (head kids) (LiteralNode Openquote "" "")), ('g' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids)))] of
                          [True, True, True] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:i:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          [True, True, False] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:g:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          [True, False, True] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:i:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          [True, False, False] -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"5lrelP:m"))):(easyCapChanges (head (tail kids))):(noRemod (drop 2 kids))
                          _ -> (head kids):(easyRegexSChanges (tail kids))

--Change captures (and nothing else) in a regex
easyCapChanges :: P5AST -> P5AST
easyCapChanges (LiteralNode Text enc uni) = (LiteralNode Text enc (regexChangeCaptures uni))
easyCapChanges (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
easyCapChanges (AbstractNode atype kids) = (AbstractNode atype kids)
easyCapChanges (Heredoc start end kids) = (Heredoc start end kids)


--noRemod gets rid of remods (since they don't exist in Perl 6)
noRemod :: [P5AST] -> [P5AST]
noRemod [] = []
noRemod kids = if (matchOnType (head kids) (LiteralNode Remod "" "")) then (tail kids) else ((head kids):(noRemod (tail kids))) 

--This function applies changeSInternals int he proper places to translate
--everything inside a regex from Perl 5 -> Perl 6
regexInternals :: P5AST -> P5AST
regexInternals (AbstractNode Op_subst kids) = (AbstractNode Op_subst (changeSInternals kids))
regexInternals (AbstractNode Op_match kids) = (AbstractNode Op_match (changeMInternals kids))
regexInternals (AbstractNode atype kids) = (AbstractNode atype (map regexInternals kids))
regexInternals (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
regexInternals (Heredoc start end kids) = (Heredoc start end kids)

--This function does the actual work of finding the regex and applying the change.
changeSInternals :: [P5AST] -> [P5AST]
changeSInternals [] = []
changeSInternals kids =  if ((length kids) >= 3) then if (and [((head ((extractUni (head kids))++" "))=='s'),(matchOnType (head kids) (LiteralNode Openquote "" "")),(matchOnType (head (drop 1 kids)) (LiteralNode Text "" "")),(matchOnType (head (drop 2 kids)) (LiteralNode Closequote "" ""))]) then [(head kids),(LiteralNode Text "1" (regexChangeCaptures (regexChange (extractUni (head (drop 1 kids)))))),(head (drop 2 kids))]++(drop 3 kids)
                                                            else ((head kids):(changeSInternals (drop 1 kids)))
                            else ((head kids):(changeSInternals (drop 1 kids)))

changeMInternals :: [P5AST] -> [P5AST]
changeMInternals [] = []
changeMInternals kids = if (matchOnType (head kids) (LiteralNode Text "" "")) then (LiteralNode Text "" (regexChangeCaptures (regexChange (extractUni (head kids))))):(drop 1 kids) else (head kids):(changeMInternals (drop 1 kids)) 

--Function to apply the regexString parser to a string
regexChange :: String -> String
regexChange instr = case parse regexString "regex" instr of
                                Left err -> error $ "\nError:\n" ++ show err ++"\nIN:\n"++instr
                                Right result -> result

--Function to apply the captureString parser to a string, aliasing all
--captures so that $1 (and friends) are still the same in Perl 6
regexChangeCaptures :: String -> String
regexChangeCaptures instr = case parse (captureString 1) "regex (Captures)" instr of
                                    Left err -> error $ "\nError:\n" ++ show err ++"\nIN:\n"++instr
                                    Right result -> result

--Parses all captures from a regex into Perl 6 aliases.
--Each capture becomes a Perl 6 aliased capture in the Perl 5
--order of captures. For the sake of simplicity and reliability, 
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
                   do{try(string "{"); countMod <- (manyTill anyToken (char '}')); return (countRegex (countMod++"@"))},     -- x{2} -> x**{2} and x{2,} -> x**{2..} and x{2,3} -> x**{2..3}
                   do{try(string "(?{"); code <- (manyTill anyToken (string "})")); return ("{"++code++"}")},                -- (?{...}) -> {...}
                   do{try(string "(?("); cond <- (manyTill regexChar (char ')')); tru <- (manyTill regexChar (char '|')); els <- (manyTill regexChar (char ')')); return ("[ "++(joinString cond)++" :: "++(joinString tru)++" | "++(joinString els)++" ]")},
                   do{try(string "(??{"); code <- (manyTill anyToken (string "})")); return ("<{"++code++"}>")},             -- (??{...}) -> <{...}>
                   do{try(do{string "|"; eof}); return "<null>"},                                                            -- /blah|/ -> /blah|<null>/
                   do{char <- anyToken; return (char:"")}]

--Wrapper for parsing the count modifier in regexs
countRegex :: String -> String
countRegex instr = case parse countString "regex count" instr of
                                Left err -> error $ "\nError:\n"++ show err ++"\nIN:\n"++instr
                                Right result -> result
                                

--Parser that translates regex counts from Perl 5 to Perl 6. Takes care of x{2} -> x**{2} and x{2,} -> x**{2..} and x{2,3} -> x**{2..3},
--as well as all of those with '?' at the end (the '?' is just left untouched).
countString :: Parser String
countString = do{firstNum <- manyTill digit (choice[char ',', char '@']);
                 spaces;
                 theRest <- choice[do{try(eof); return ""},
                                   do{try(char '@'); return "..."},
                                   do{secondNum <- manyTill digit (char '@'); return (".."++secondNum)}];
                 return ("**{"++firstNum++theRest++"}")
                }

--When the split is implicitly on the topic ($_), it can become a method call (.split(...))
topicSplit :: P5AST -> P5AST
topicSplit (AbstractNode Op_split kids) = (AbstractNode Op_split (topicMethod kids))
topicSplit (AbstractNode atype kids) = (AbstractNode atype (map topicSplit kids))
topicSplit (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
topicSplit (Heredoc start end kids) = (Heredoc start end (map topicSplit kids))

--Actually convert to a method.
topicMethod :: [P5AST] -> [P5AST]
topicMethod [] = []
topicMethod kids = if (not (isIn (AbstractNode Listelem []) kids)) then [(LiteralNode Junk "" "")] else kids

--translates foreach loops:
--foreach $foo (@bar) {...} becomes foreach @bar -> $foo {...}
foreachTranslation :: P5AST -> P5AST
foreachTranslation (AbstractNode Op_leaveloop kids) = if (isIn (LiteralNode Token "1" "foreach") kids) then (AbstractNode Op_leaveloop (newForeach kids)) else (AbstractNode Op_leaveloop (map foreachTranslation kids))                                             
foreachTranslation (AbstractNode atype kids) = (AbstractNode atype (map foreachTranslation kids))
foreachTranslation (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
foreachTranslation (Heredoc start end kids) = (Heredoc start end kids)

--Does the dirty work of actual translation on foreach
newForeach :: [P5AST] -> [P5AST]
newForeach [] = []
newForeach kids = if (matchOnType (head kids) (LiteralNode Junk "" "")) then (head kids):(newForeach (drop 1 kids)) else 
            case [(isIn (AbstractNode Op_padsv []) kids), (isIn (AbstractNode Op_padav []) kids), (isIn (AbstractNode Op_list []) kids), (isIn (AbstractNode Op_rv2av []) kids), (isIn (AbstractNode Op_gv []) kids), (isIn (LiteralNode Declarator "" "my") kids), (isIn (AbstractNode Op_rv2gv []) kids)] of
                    [True, True, False, False, False, False, False]  -> (map foreachTranslation [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_padav []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Declarator "1" "my"),(extractNodetype (AbstractNode Op_padsv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)])
                    [True, False, True, False, False, True, False]  -> (map foreachTranslation [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "1" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "1" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Declarator "1" "my"),(extractNodetype (AbstractNode Op_padsv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)])
                    [False, False, False, True, False, False, False] -> (map foreachTranslation [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2av []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2gv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)])
                    [False, False, True, False, False, False, True] -> (map foreachTranslation [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2gv []) kids),(extractNodetype (AbstractNode Op_lineseq []) kids)])
                    [False, False, True, False, False, False, False] -> (map foreachTranslation [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(extractNodetype (AbstractNode Op_rv2av []) kids),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Sigil "" "$_"),(extractNodetype (AbstractNode Op_lineseq []) kids)])
                    [False, False, True, False, True, False, False] -> (map foreachTranslation [(LiteralNode Token "1" "foreach"),(LiteralNode Junk "1" " "),(LiteralNode Opener "1" "("),(extractNodetype (AbstractNode Op_list []) kids),(LiteralNode Closer "1" ")"),(AbstractNode Op_iter []),(LiteralNode Junk "1" " "),(LiteralNode Operator "1" "->"),(LiteralNode Junk "1" " "),(LiteralNode Sigil "" "$_"),(extractNodetype (AbstractNode Op_lineseq []) kids)])
                    _                           -> []


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
changeCloseMethod kids = case (matchWithoutEnc (head kids) (LiteralNode Operator "" "close")) of
                           True  -> [(head (tail kids)), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "close")])])]++(drop 2 kids)
                           False -> (head kids):(changeCloseMethod (drop 1 kids))


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
lengthToMethod (AbstractNode Op_length kids) = (AbstractNode Op_length (toCharMethod kids))
lengthToMethod (AbstractNode atype kids) = (AbstractNode atype (map lengthToMethod kids))
lengthToMethod (Heredoc start end kids) = (Heredoc start end kids)
lengthToMethod (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Actually performs change to method call
toCharMethod :: [P5AST] -> [P5AST]
toCharMethod [] = []
toCharMethod kids = if ((length kids) >= 2) then case [(matchWithoutEnc (head kids) (LiteralNode Operator "" "length")), (matchWithoutEnc (head (drop 1 kids)) (LiteralNode Opener "" "("))] of
                                             [True, True]   -> [(head (drop 2 kids)), (LiteralNode Operator "" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "chars")])])]++(drop 4 kids)
                                             [True, False]  -> [(head (drop 1 kids)),(LiteralNode Operator "" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "chars")])])]++(drop 2 kids)
                                             [False, True]  -> (head kids):(toCharMethod (drop 1 kids))
                                             [False, False] -> (head kids):(toCharMethod (drop 1 kids))
                                                               else kids

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
--readlineTranslate (AbstractNode Op_readline kids) = (AbstractNode Op_readline [(LiteralNode Sigil "1" ('$':(drop 1 (reverse (drop 1 (reverse (extractUni (head kids)))))))), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "readline")])])])
readlineTranslate (AbstractNode Op_readline kids) = (AbstractNode Op_readline (readlineMethod kids))
readlineTranslate (AbstractNode atype kids) = (AbstractNode atype (map readlineTranslate kids))
readlineTranslate (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
readlineTranslate (Heredoc start end kids) = (Heredoc start end kids)

readlineMethod :: [P5AST] -> [P5AST]
readlineMethod [] = []
readlineMethod kids = case (matchOnType (head kids) (LiteralNode Token "" "")) of
                        True  -> if ((length kids) >= 2) then case ((head (tail (extractUni (head (tail kids)))))=='$') of 
                                   True  -> [(LiteralNode Sigil "" (drop 1 (reverse (drop 1 (reverse (extractUni (head kids))))))), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "readline")])])]++(drop 1 kids)
                                   False -> [(LiteralNode Sigil "" ('$':(drop 1 (reverse (drop 1 (reverse (extractUni (head kids)))))))), (LiteralNode Operator "1" "."), (AbstractNode Op_method [(AbstractNode Op_const [(LiteralNode Token "1" "readline")])])]++(drop 1 kids)
                                            else kids
                        False -> (head kids):(readlineMethod (drop 1 kids))

regexModifiers :: P5AST -> P5AST
regexModifiers (AbstractNode Op_subst kids) = case [(matchOnType (LiteralNode Remod "" "") (extractNodetype (LiteralNode Remod "" "") kids)), ('g' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids))), ('e' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids)))] of
                                                [True, True, True, True]    -> (AbstractNode Op_subst (dropEMod (moveGMod (moveIMod kids))))
                                                [True, True, True, False]   -> (AbstractNode Op_subst (moveGMod (moveIMod kids)))
                                                [True, True, False, True]   -> (AbstractNode Op_subst (moveGMod (dropEMod kids)))
                                                [True, False, True, True]   -> (AbstractNode Op_subst (moveIMod (dropEMod kids)))
                                                [True, True, False, False]  -> (AbstractNode Op_subst (moveGMod kids))
                                                [True, False, False, True]  -> (AbstractNode Op_subst (dropEMod kids))
                                                [True, False, True, False]  -> (AbstractNode Op_subst(moveIMod kids))
                                                [True, False, False, False] -> (LiteralNode Text "" "FOUND IT!")
                                                _                           -> (AbstractNode Op_subst kids)
regexModifiers (AbstractNode Op_match kids) = case [(matchOnType (LiteralNode Remod "" "") (extractNodetype (LiteralNode Remod "" "") kids)), ('g' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids)))] of
                                                [True, True, True]    -> (AbstractNode Op_subst (moveIMod (moveGMod kids)))
                                                [True, True, False]   -> (AbstractNode Op_subst (moveIMod kids))
                                                [True, False, True]   -> (AbstractNode Op_subst (moveGMod kids))
                                                _                           -> (AbstractNode Op_subst kids)
regexModifiers (AbstractNode Op_split kids) = case [(matchOnType (LiteralNode Remod "" "") (extractNodetype (LiteralNode Remod "" "") kids)), ('g' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids))), ('i' `elem` (extractUni (extractNodetype (LiteralNode Remod "" "") kids)))] of
                                                [True, True, True]    -> (AbstractNode Op_subst (moveIMod (moveGMod kids)))
                                                [True, True, False]   -> (AbstractNode Op_subst (moveIMod kids))
                                                [True, False, True]   -> (AbstractNode Op_subst (moveGMod kids))
                                                _                           -> (AbstractNode Op_subst kids)
regexModifiers (AbstractNode atype kids) = (AbstractNode atype (map regexModifiers kids))
regexModifiers (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
regexModifiers (Heredoc start end kids) = (Heredoc start end kids)

moveIMod :: [P5AST] -> [P5AST]
moveIMod [] = []
moveIMod kids = case [(matchOnType (head kids) (LiteralNode Openquote "" "")), (matchOnType (head kids) (LiteralNode Remod "" ""))] of
                  [True, False]  -> (LiteralNode Openquote "" ((head (reverse (extractUni (head kids)))):"i:"++(tail (reverse (extractUni (head kids))))) ):(drop 1 kids)
                  [False, True]  -> (LiteralNode Remod "" (removeChar 'i' (extractUni (head kids)))):(drop 1 kids)
                  [False, False] -> (head kids):(moveIMod (tail kids))

moveGMod :: [P5AST] -> [P5AST]
moveGMod [] = []
moveGMod kids = case [(matchOnType (head kids) (LiteralNode Openquote "" "")), (matchOnType (head kids) (LiteralNode Remod "" ""))] of
                  [True, False]  -> (LiteralNode Openquote "" (reverse ((head (reverse (extractUni (head kids)))):"g:"++(tail (reverse (extractUni (head kids)))))) ):(moveGMod (drop 1 kids))
                  [False, True]  -> (LiteralNode Remod "" (removeChar 'g' (extractUni (head kids)))):(drop 1 kids)
                  [False, False] -> (head kids):(moveGMod (tail kids))

removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar todrop instr = case ((head instr)==todrop) of
                  True  -> (tail instr)
                  False -> (head instr):(removeChar todrop (tail instr))


dropEMod :: [P5AST] -> [P5AST]
dropEMod [] = []
dropEMod kids = case [(matchOnType (head kids) (LiteralNode Text "" "")), (matchOnType (head kids) (LiteralNode Remod "" ""))] of
                  [True, False] -> (head kids):(head (drop 1 kids)):(LiteralNode Opener "" "{ "):(head (drop 2 kids)):(LiteralNode Opener "" " }"):(dropEMod (drop 3 kids))
                  [False, True] -> (LiteralNode Remod "" (removeChar 'e' (extractUni (head kids)))):(drop 1 kids)
                  _             -> (head kids):(dropEMod (tail kids))

{-Translates =~ -> ~~ for using regexs in P6
The name of the function is a bit long, but it won't be called often
and at least it's very descriptive -}
equalTildeToTildeTilde :: P5AST -> P5AST
equalTildeToTildeTilde (LiteralNode Operator enc "=~") = (LiteralNode Operator enc "~~")
equalTildeToTildeTilde (AbstractNode atype kids) = (AbstractNode atype (map equalTildeToTildeTilde kids))
equalTildeToTildeTilde (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
equalTildeToTildeTilde (Heredoc start end kids) = (Heredoc start end kids)

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
arrayKey (LiteralNode Sigil enc uni) = if ('[' `elem` uni) then (scalarSigilToArraySigil (LiteralNode Sigil enc uni)) else (LiteralNode Sigil enc uni)
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
scalarSigilToArraySigil (LiteralNode Sigil enc uni) = (LiteralNode Sigil enc ('@':(drop 1 uni)))
scalarSigilToArraySigil (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
scalarSigilToArraySigil (AbstractNode atype kids) = (AbstractNode atype kids)
scalarSigilToArraySigil (Heredoc start end kids) = (Heredoc start end kids)

{-Do changes to hashes with keys, such as $hash{word}->%hash<word> 
and $hash{$var}->%hash{$var}-}
hashKey :: P5AST -> P5AST
hashKey (AbstractNode Op_helem kids) = if (isInOrder [(AbstractNode Op_rv2hv []), (LiteralNode Opener "1" "{"), (AbstractNode Op_const []), (LiteralNode Closer "1" "}")] kids) 
                                              then (AbstractNode Op_helem (map constHashChanges kids)) else
                                              if (isInOrder [(AbstractNode Op_rv2hv []), (LiteralNode Opener "1" "{"), (LiteralNode Closer "1" "}")] kids)
                                                then (AbstractNode Op_helem (map hashChanges kids)) 
                                                else (AbstractNode Op_helem (map hashKey kids)) 
hashKey (LiteralNode Sigil enc uni) = case [('{' `elem` uni), (and [((head uni)=='$'), ((head (tail uni))=='{'), ((head (drop 2 uni))=='^')])] of
                                                         [True, True]  -> (LiteralNode Sigil enc ("$*"++(drop 3 (reverse (tail (reverse uni))))))
							 [True, False] -> (LiteralNode Sigil enc ('%':(tail uni)))
							 _                 -> (LiteralNode Sigil enc uni)
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
scalarSigilToHashSigil (LiteralNode Sigil enc uni) = (LiteralNode Sigil enc ('%':(drop 1 uni)))
scalarSigilToHashSigil (LiteralNode atype enc uni) = (LiteralNode atype enc uni)
scalarSigilToHashSigil (AbstractNode atype kids) = (AbstractNode atype kids)

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
    if ('n' `elem` options) then (putStrLn "Not Translating...")  else case [('o' `elem` options), ('r' `elem` options)] of 
            [True, True]   -> putStrLn ("Translating with the heavy object oriented option and limited regex support...")
            [True, False]  -> putStrLn ("Translating with the heavy object oriented option...")
            [False, True]  -> putStrLn ("Translating with limited regex support...")
            [False, False] -> putStrLn ("Translating...")
    let tree = (translate (AbstractNode P5AST ast) options)
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
getModifiers [] = " "
getModifiers args = case (head args) of
                         "-Oo"    ->  ('o':(getModifiers (drop 1 args)))
                         "-V"     ->  ('v':(getModifiers (drop 1 args)))
                         "-U"     ->  ('u':(getModifiers (drop 1 args)))
                         "-R"     ->  ('r':(getModifiers (drop 1 args)))
                         "-N"     ->  ('n':(getModifiers (drop 1 args)))
                         _        ->  (' ':(getModifiers (drop 1 args)))

--getFirstFile (oddly enough) gets the first file (which will be the second to last argument). 
getFirstFile :: [String] -> String
getFirstFile [] = ""
getFirstFile args = (head (drop 1 (reverse args))) 

--getSecondFile gets the second filename from the args. It should always be the last arument.
getSecondFile :: [String] -> String
getSecondFile [] = ""
getSecondFile args = (head (reverse args))
