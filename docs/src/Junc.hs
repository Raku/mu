import Junc
import AST
import Data.Set
import Data.List

jun1 = VJunc $ Junc JAny emptySet (mkSet [VInt 24, VInt 22])
jun2 = VJunc $ Junc JNone emptySet emptySet
jun3 = VJunc $ Junc JAll emptySet (mkSet [VStr "hi", VBool False])
jun4 = VJunc $ Junc JAll emptySet (mkSet [VStr "ho", VBool True])

-- play with opJuncAll

mydat = [VInt 12, jun1, jun2, jun3, jun4]
myall = opJuncAll mydat


-- test juncTypeIs

myv1 = juncTypeIs jun1 [JAll, JNone]
myv2 = juncTypeIs jun4 [JAll, JNone]

-- test mergeJunc 

mylis = [4,6,1,5,2,9,2,4,5,2]
mylis_gs = mkSet [ v | [v] <- group $ sort mylis ]
mylis_n  = mkSet $ nub mylis 
-- aha! they are not the same

myds = [VInt 4, VInt 5]
myvs = [VInt 1, VInt 2, VInt 3, VInt 3] 

myany = mergeJunc JAny myds myvs
myone = mergeJunc JOne myds myvs

-- create a list whose elements are thoese elements of mylis
-- which are repeated more than once
dups = [ v | (v:_:_) <- group $ sort mylis ]
dups2 = [ vs | vs <- group $ sort mylis, length vs > 1 ]
dups3 = [ vs | vs <- group $ sort mylis, not $ null $ tail vs ]
