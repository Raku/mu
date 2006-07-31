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

--Returns the first instance of a given type of node from a list of nodes. 
extractNodetype :: P5AST -> [P5AST] -> P5AST
extractNodetype _ [] = (AbstractNode UnknownAbs [])
extractNodetype node nlist = if (matchOnType node (head nlist)) then (head nlist) else (extractNodetype node (tail nlist))

--Get just the kids of a given node ([] in case of literal node).
extractKids :: P5AST -> [P5AST]
extractKids (AbstractNode atype kids) = kids
extractKids (Heredoc start end kids) = kids
extractKids (LiteralNode _ _ _) = []

--When called on the kids of a node that make up a function call, this function extracts the second arguement (the first thing after a comma)
getSecondArg :: [P5AST] -> P5AST
getSecondArg [] = (AbstractNode UnknownAbs [])
getSecondArg list = if (matchWithoutEnc (head list) (AbstractNode Listelem [])) then (dropComma (head list)) else (getSecondArg (tail list))

--Drops the comma from an argument to a function (since the literal comma gets stuck in with with argument)
dropComma :: P5AST -> P5AST
dropComma (AbstractNode Listelem kids) = (head (tail kids))
dropComma (AbstractNode atype kids) = (AbstractNode atype kids)
dropComma (Heredoc start end kids) = (Heredoc start end kids)
dropComma (LiteralNode atype enc uni) = (LiteralNode atype enc uni)

--Extracts the first text node (based only on type, not other fields)
extractText :: [P5AST] -> P5AST
extractText [] = (LiteralNode Text "1" "")
extractText kids = if ((getLType (head kids))==Text) then (head kids) else (extractText (tail kids)) 

--Match based only on subtype
matchOnType :: P5AST -> P5AST -> Bool
matchOnType (AbstractNode atype1 kids1) (AbstractNode atype2 kids2) = if (atype1==atype2) then True else False
matchOnType (LiteralNode atype1 _ _) (LiteralNode atype2 _ _) = if (atype1==atype2) then True else False
matchOnType _ _ = False 

--Get the type of a literal node
getLType :: P5AST -> LitType
getLType (AbstractNode sometype kids) = UnknownLit
getLType (Heredoc start end kids) = UnknownLit
getLType (LiteralNode sometype enc uni) = sometype

--Join a list of lists of nodes into a list of nodes
join :: [[P5AST]] -> [P5AST]
join [] = []
join lists = (head lists)++(join (tail lists))

--Turn a list of strings into a single string
joinString :: [String] -> String
joinString [] = []
joinString strs = (head strs)++(joinString (tail strs))

--Extract the uni field from a node
extractUni :: P5AST -> String
extractUni (LiteralNode _ _ uni) = uni
extractUni (AbstractNode _ _) = "" 
extractUni (Heredoc _ _ _) = "" 

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

--Like isInOrder, except the nodes have to follow directly after one another.
isInSequence :: [P5AST] -> [P5AST] -> Bool
isInSequence _ [] = True
isInSequence [] _ = False
isInSequence nodes list = if (allMatch nodes list) then True else (isInSequence (tail nodes) list) 

--Make sure two lists match
allMatch :: [P5AST] -> [P5AST] -> Bool
allMatch [] [] = True
allMatch _ [] = False
allMatch [] _ = False
allMatch list1 list2 = if (matchWithoutEnc (head list1) (head list2)) then (allMatch (tail list1) (tail list2)) else False

{-Matches nodes based on type (Heredoc, Abstract or Literal) subtype (Junk, Op_leave, PNothing, etc)
and (in the case of literal nodes) on the uni field. Used in the above search functions.-}
matchWithoutEnc :: P5AST -> P5AST -> Bool
matchWithoutEnc (LiteralNode type1 _ uni1) (LiteralNode type2 _ uni2) = if (and [(uni1==uni2), (type1==type2)]) then True else False
matchWithoutEnc (AbstractNode type1 kids1) (AbstractNode type2 kids2) = if (type1 == type2) then True else False
matchWithoutEnc (Heredoc start1 end1 kids1) (Heredoc start2 end2 kids2) = if (and [(matchWithoutEnc start1 start2),(matchWithoutEnc end1 end2)]) then True else False
matchWithoutEnc _ _ = False