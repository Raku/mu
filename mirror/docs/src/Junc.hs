import Junc
import AST
import Data.Set
import Data.List

jun1 = VJunc $ Junc JAny emptySet (mkSet [VInt 24, VInt 22])
jun2 = VJunc $ Junc JNone emptySet emptySet
jun3 = VJunc $ Junc JAll emptySet (mkSet [VStr "hi", VBool False])
jun4 = VJunc $ Junc JAll emptySet (mkSet [VStr "ho", VBool True])
jun5 = VJunc $ Junc JOne emptySet emptySet

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

{- 

[05:37] * metaperl wonders if GHC is smart enough to interpret (length list > 1) as not . null $ drop 1 list
[05:37] <TheHunter> no, that's semantically different.
[05:37] <lightstep> how are they different (modulo termination)?
[05:37] <metaperl> but from a functional viewpoint, they return True/False in the same cases...
[05:37] <tromp> if you consider length (0:0:undefined)
[05:37] <TheHunter> lenght [1..] > 1 === _|_, not $ null $ drop  1 [1..] === True.
[05:37] *** arjanb (arjanb@borganism.student.utwente.nl) joined
[05:38] <metaperl> oh, some tricky edge cases

-}

-- play with ApplyArg isPartialJunc isTotalJunc

aa  = ApplyArg "somename" jun2 False
aap = isPartialJunc aa
aat = isTotalJunc   aa

bb = ApplyArg "somename" jun1 False
bbp = isPartialJunc bb
bbt = isTotalJunc   bb

{-
I have written a program to run a test suite for a list of functions:

    [isTotalJunc, isPartialJunc]

where each function receives a datum of type ApplyArg whose value slot is 
one element of the list of types below at a time:

    [JNone, JOne, JAny, JAll] 

I therefore must run 8 tests (the cross product of functions and types).

Right now, here is the output:

*Main> test_total_partial
["fun: (->)input: JNoneoutput: True","fun: (->)input: JOneoutput: False","fun: (->)input: JAnyoutput: False","fun: (->)input: JAlloutput: True","fun: (->)input: JNoneoutput: False","fun: (->)input: JOneoutput: True","fun: (->)input: JAnyoutput: True","fun: (->)input: JAlloutput: False"]

One problem is that functions do not print as their name. The call to (show f)
in showres simply yields (->)

Another problem is that I cannot manage to separate each string with a
carriage return.

Any help is appreciated.

-}

{- SORJE's function:

funs  = [(isTotalJunc, "isTotalJunc"), (isPartialJunc, "isPartialJunc")]
types = [JNone, JOne, JAny, JAll]                           

mkdat typ = ApplyArg "somename" (VJunc $ Junc typ emptySet emptySet) False

main =  mapM tester funs


tester (f,fn) = do putStrLn $ "func: " ++ fn
                   mapM (\x -> putStrLn $ show x ++ "\t" ++ (show.f.mkdat) x) types



-}

test_total_partial = let funs  = [isTotalJunc, isPartialJunc]
			 types = [JNone, JOne, JAny, JAll] 
			 mkdat typ = ApplyArg "somename" (VJunc $ Junc typ emptySet emptySet) False
		         showres f t r = "fun: " ++ (show f) ++ "input: " ++ (show t) ++ "output: " ++ (show r)
		         runtest f t = do {
					   retval <- f (mkdat t);
					   return $ (showres f t retval)
					  }
			 in
  putStr $ unlines [ (showres f t (f (mkdat t))) | f <- funs, t <- types ]


main = undefined
