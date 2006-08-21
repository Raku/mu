module ASTUtil where
{-----------------------------
This is a collection of utility functions I've made. Most of them are
nothing special, but they do make things easier and cleaner in the rest 
of the code. Many of them imitate more general functions specialized for
AST nodes.

See ASTTranslate.hs for more.

Originally created by Sage LaTorra for Summer of Code 2006.
------------------------------}
import ASTDefinition

{-Removes a given character from a string.-}

removeChar :: Char -> String -> String
removeChar _ []         = []
removeChar todrop instr = case ((head instr)==todrop) of
    True  -> (tail instr)
    False -> (head instr):(removeChar todrop (tail instr))


--Makes a list of strings from a list of lists of strings.

makeList :: [[String]] -> [String]
makeList []    = []
makeList alist = (head alist)++(makeList (tail alist))


--Take a string and return a list of strings with each element being a word in that string. 
--The second argument is what's already been collected (allowing you to start the list of words with your own list, if need be)
--To just get the contents of the string broken into words, just call makeWords instring [""]

makeWords :: String -> [String] -> [String]
makeWords [] curout   = curout
makeWords inst curout = if ((head inst)==' ') then (makeWords (drop 2 inst) ([(head (tail inst))]:curout)) else (makeWords (tail inst) (((head curout)++[(head inst)]):(tail curout)))


--drop the leading character if it matches the first argument of the call

dropLeadingChar :: Char -> String -> String
dropLeadingChar _ []           = []
dropLeadingChar todrop astring = if ((head astring)==todrop) then (tail astring) else astring 


--Returns the first instance of a given type of node from a list of nodes. 

extractNodetype :: P5AST -> [P5AST] -> P5AST
extractNodetype _ []       = (AbstractNode "UnknownAbs" [])
extractNodetype node nlist = if (matchOnType node (head nlist)) then (head nlist) else (extractNodetype node (tail nlist))


extractNodetypeSkip :: Int -> P5AST -> [P5AST] -> P5AST
extractNodetypeSkip 0 node nlist = extractNodetype node nlist
extractNodetypeSkip _ _ []      = (AbstractNode "UnknownAbs" [])
extractNodetypeSkip skips node nlist = if (matchOnType node (head nlist)) then (extractNodetypeSkip (skips-1) node (tail nlist)) else (extractNodetypeSkip skips node (tail nlist))

--Get just the kids of a given node ([] in case of literal node).

extractKids :: P5AST -> [P5AST]
extractKids (AbstractNode atype kids)    = kids
extractKids (Heredoc doc start end kids) = kids
extractKids (LiteralNode _ _ _)          = []


--When called on the kids of a node that make up a function call, this function extracts the second arguement (the first thing after a comma)

getSecondArg :: [P5AST] -> P5AST
getSecondArg []   = (AbstractNode "UnknownAbs" [])
getSecondArg list = if (matchWithoutEnc (head list) (AbstractNode "listelem" [])) then (dropComma (head list)) else (getSecondArg (tail list))


--Drops the comma from an argument to a function (since the literal comma gets stuck in with with argument)

dropComma :: P5AST -> P5AST
dropComma (AbstractNode "listelem" kids) = (head (tail kids))
dropComma (AbstractNode atype kids)      = (AbstractNode atype kids)
dropComma (Heredoc doc start end kids)   = (Heredoc doc start end kids)
dropComma (LiteralNode atype enc uni)    = (LiteralNode atype enc uni)


--Extracts the first text node (based only on type, not other fields)

extractText :: [P5AST] -> P5AST
extractText []   = (LiteralNode "text" "1" "")
extractText kids = if ((getLType (head kids))=="text") then (head kids) else (extractText (tail kids)) 


--Match based only on subtype

matchOnType :: P5AST -> P5AST -> Bool
matchOnType (AbstractNode atype1 kids1) (AbstractNode atype2 kids2) = if (atype1==atype2) then True else False
matchOnType (LiteralNode atype1 _ _) (LiteralNode atype2 _ _)       = if (atype1==atype2) then True else False
matchOnType _ _                                                     = False 


--Get the type of a literal node

getLType :: P5AST -> String
getLType (AbstractNode sometype kids)   = "UnknownLit"
getLType (Heredoc _ start end kids)     = "UnknownLit"
getLType (LiteralNode sometype enc uni) = sometype


--Join a list of lists of nodes into a list of nodes

join :: [[P5AST]] -> [P5AST]
join []    = []
join lists = (head lists)++(join (tail lists))


--Turn a list of strings into a single string

joinString :: [String] -> String
joinString []   = []
joinString strs = (head strs)++(joinString (tail strs))


--Extract the uni field from a node

extractUni :: P5AST -> String
extractUni (LiteralNode _ _ uni) = uni
extractUni (AbstractNode _ _)    = "" 
extractUni (Heredoc _ _ _ _)     = "" 


{-A simple search convenience function, returns true when the given node is in the list
matches based on type (Abstract or Literal) subtype (Junk, Op_leave, Sigil, etc.) and, in
the case of literal nodes, the uni part of the node.-}

isIn :: P5AST -> [P5AST] -> Bool
isIn _ []      = False
isIn node list = if (matchWithoutEnc (head list) node) then True else (isIn node (tail list))


{-Searches for the given list of nodes in another list allows any number of nodes between the nodes being searched for matches like isIn
Useage: isInOrder [NodesBeingSearchedFor] [SearchTarget]-}

isInOrder :: [P5AST] -> [P5AST] -> Bool
isInOrder [] []      = True
isInOrder _ []       = False
isInOrder [] _       = True
isInOrder nodes list = if (matchWithoutEnc (head list) (head nodes)) then (isInOrder (tail nodes) (tail list)) else (isInOrder nodes (tail list))


--Like isInOrder, except the nodes have to follow directly after one another.

isInSequence :: [P5AST] -> [P5AST] -> Bool
isInSequence _ []       = True
isInSequence [] _       = False
isInSequence nodes list = if (allMatch nodes list) then True else (isInSequence (tail nodes) list) 


{-This function is just like isInSequence, but it only matches on type -}

isInSequenceType :: [P5AST] -> [P5AST] -> Bool
isInSequenceType _ []       = True
isInSequenceType [] _       = False
isInSequenceType nodes list = if (allMatchType nodes list) then True else (isInSequence (tail nodes) list)


{-Just like allMatch, but only matching on type-}

allMatchType :: [P5AST] -> [P5AST] -> Bool
allMatchType [] [] = True
allMatchType _ [] = False
allMatchType [] _ = False
allMatchType list1 list2 = case [(matchOnType (head list1) (LiteralNode "junk" "" "")), (matchOnType (head list2) (LiteralNode "junk" "" ""))] of
    [True, True]   -> (allMatch (tail list1) (tail list2))
    [True, False]  -> (allMatch (tail list1) list2)
    [False, True]  -> (allMatch list1 (tail list2))
    [False, False] -> if (matchOnType (head list1) (head list2)) then (allMatch (tail list1) (tail list2)) else False


--Make sure two lists match

allMatch :: [P5AST] -> [P5AST] -> Bool
allMatch [] []       = True
allMatch _ []        = False
allMatch [] _        = False
allMatch list1 list2 = case [(matchOnType (head list1) (LiteralNode "junk" "" "")), (matchOnType (head list2) (LiteralNode "junk" "" ""))] of
    [True, True]   -> (allMatch (tail list1) (tail list2))
    [True, False]  -> (allMatch (tail list1) list2)
    [False, True]  -> (allMatch list1 (tail list2))
    [False, False] -> if (matchWithoutEnc (head list1) (head list2)) then (allMatch (tail list1) (tail list2)) else False


{-Matches nodes based on type (Heredoc, Abstract or Literal) subtype (Junk, Op_leave, PNothing, etc)
and (in the case of literal nodes) on the uni field. Used in the above search functions.-}

matchWithoutEnc :: P5AST -> P5AST -> Bool
matchWithoutEnc (LiteralNode type1 _ uni1) (LiteralNode type2 _ uni2)             = if (and [(uni1==uni2), (type1==type2)]) then True else False
matchWithoutEnc (AbstractNode type1 kids1) (AbstractNode type2 kids2)             = if (type1 == type2) then True else False
matchWithoutEnc (Heredoc doc1 start1 end1 kids1) (Heredoc doc2 start2 end2 kids2) = if (and [(matchWithoutEnc start1 start2),(matchWithoutEnc end1 end2), doc1==doc2]) then True else False
matchWithoutEnc _ _                                                               = False
