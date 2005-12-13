{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Prims where
import PIL.Native.Types
import PIL.Native.Coerce
import qualified Data.Map as NMap

{-| 

PIL.Native.Prims

This module defines the methods available for our Native types. 

NOTE: All types respond to the methods of Any

Any
  as_bit  ()        -> Bit
  as_int  ()        -> Int
  as_num  ()        -> Num    
  as_str  ()        -> Str
  is_nil  ()        -> Bit  
  not_nil ()        -> Bit
  eq      ()        -> Bit
  lt      ()        -> Bit      
  le      ()        -> Bit  
  gt      ()        -> Bit  
  ge      ()        -> Bit  
  
Bit
  not ()            -> Bit
  and (Block)       -> Bit
  or  (Block)       -> Bit

Int 
  increment ()      -> Int
  decrement ()      -> Int   
  add       (Int)   -> Int
  subtract  (Int)   -> Int      
  multiply  (Int)   -> Int         
  divide    (Int)   -> Int    
                    
Num                 
  increment ()      -> Num
  decrement ()      -> Num   
  add       (Num)   -> Num
  subtract  (Num)   -> Num      
  multiply  (Num)   -> Num         
  divide    (Num)   -> Num         

Str 
  length ()         -> Num
  concat (Str)      -> Str
  fetch  ()         -> Str
  store  (Str)      -> Nil
  
List 
  length ()         -> Num
  concat (Any)      -> Nil
  fetch  (Num)      -> Any
  store  (Num, Any) -> Nil 
  push   (List)     -> Nil

Hash 
  length ()         -> Num
  keys   ()         -> List
  values ()         -> List
  concat (Any)      -> Nil
  fetch  (Any)      -> Any
  store  (Any, Any) -> Nil
  push   (Hash)     -> Nil  

See Also:

  PIL.Native.Types
  PIL.Native.Coerce

-}

type Prims = MapOf (Native -> Native)

anyPrims :: MapOf (Native -> SeqOf Native -> Native)
anyPrims = mkMap
    [ prim0 "as_bit"    (fromNative :: Native -> NativeBit)
    , prim0 "as_int"    (fromNative :: Native -> NativeInt)
    , prim0 "as_num"    (fromNative :: Native -> NativeNum)
    , prim0 "as_str"    (fromNative :: Native -> NativeStr)
    , prim0 "is_nil"    is_nil
    , prim0 "not_nil"   (not . is_nil)
    , prim1 "eq"        (==)
    , prim1 "lt"        (<)
    , prim1 "le"        (<=)
    , prim1 "gt"        (>)
    , prim1 "ge"        (>=)
    ]
    where
    is_nil (NError {})  = True
    is_nil _            = False

bitPrims :: MapOf (NativeBit -> SeqOf Native -> Native)
bitPrims = mkMap
    [ prim0 "not"  not
    , prim1 "and"  (&&)
    , prim1 "or"   (||)
    ]

intPrims :: MapOf (NativeInt -> SeqOf Native -> Native)
intPrims = mkMap
    [ prim0 "increment"  succ
    , prim0 "decrement"  pred
    , prim1 "add"        (+)
    , prim1 "subtract"   (-)
    , prim1 "multiply"   (*)
    , prim1 "divide"     div
    ]

numPrims :: MapOf (NativeNum -> SeqOf Native -> Native)
numPrims = mkMap
    [ prim0 "increment"  succ
    , prim0 "decrement"  pred
    , prim1 "add"        (+)
    , prim1 "subtract"   (-)
    , prim1 "multiply"   (*)
    , prim1 "divide"     (/)
    ]

strPrims :: MapOf (NativeStr -> SeqOf Native -> Native)
strPrims = mkMap
    [ prim0 "length"     (size)
    , prim1 "concat"     (append)
    , prim1 "fetch"      (fetch)
    , prim2 "store"      (insert)
    ]

seqPrims :: MapOf (NativeSeq -> SeqOf Native -> Native)
seqPrims = mkMap
    [ prim0 "length"     (size)
    , prim1 "concat"     (append)
    , prim1 "fetch"      (fetch)
    , prim2 "store"      (insert)
    , primX "push"       (push)
    ]

mapPrims :: MapOf (NativeMap -> SeqOf Native -> Native)
mapPrims = mkMap
    [ prim0 "length"     (size)
    , prim0 "keys"       (indices)
    , prim0 "values"     (elems)
    , prim1 "concat"     (append)
    , prim1 "fetch"      (fetch)
    , prim2 "store"      (insert)
    , primX "push"       (pushHash)
    ]
    where
    pushHash :: NativeMap -> SeqOf Native -> NativeMap
    pushHash obj args = obj `NMap.union` (NMap.fromList $ listHash $ elems args)
    listHash [] = []
    listHash (k:v:xs) = (fromNative k, v):(listHash xs)
    listHash _ = error "odd number of elements for hash"

blockPrims :: MapOf (NativeSub -> SeqOf Native -> Native)
blockPrims = empty

prim0 :: (IsNative inv, IsNative ret)
    => String
    -> (inv -> ret)
    -> (String, inv -> SeqOf Native -> Native)
prim0 str f = (str, f')
    where
    f' obj args | isEmpty args = toNative (f obj)
    f' _   _    = errArgs

prim1 :: (IsNative inv, IsNative a, IsNative ret)
    => String
    -> (inv -> a -> ret)
    -> (String, inv -> SeqOf Native -> Native)
prim1 str f = (str, f')
    where
    f' obj args | size args == 1 =
        toNative $ f obj (fromNative $ args ! 0)
    f' _   _    = errArgs

prim2 :: (IsNative inv, IsNative a, IsNative b, IsNative ret)
    => String
    -> (inv -> a -> b -> ret)
    -> (String, inv -> SeqOf Native -> Native)
prim2 str f = (str, f')
    where
    f' obj args | size args == 2 =
        toNative $ f obj (fromNative $ args ! 0) (fromNative $ args ! 1)
    f' _   _    = errArgs

primX :: (IsNative inv, IsNative ret)
    => String
    -> (inv -> SeqOf Native -> ret)
    -> (String, inv -> SeqOf Native -> Native)
primX str f = (str, f')
    where
    f' obj args = toNative $ f obj args

errArgs :: forall a. a
errArgs = error "Invalid number of arguments"


