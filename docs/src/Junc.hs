import Junc
import AST
import Data.Set

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

