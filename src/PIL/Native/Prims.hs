{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Prims where
import PIL.Native.Types
import PIL.Native.Coerce

type Prims = MapOf (Native -> Native)

anyPrims :: MapOf (Native -> [Native] -> Native)
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

bitPrims :: MapOf (NativeBit -> [Native] -> Native)
bitPrims = mkMap
    [ prim0 "not"  not
    , prim1 "and"  (&&)
    , prim1 "or"   (||)
    ]

intPrims :: MapOf (NativeInt -> [Native] -> Native)
intPrims = mkMap
    [ prim0 "increment"  succ
    , prim0 "decrement"  pred
    , prim1 "add"        (+)
    , prim1 "subtract"   (-)
    , prim1 "multiply"   (*)
    , prim1 "divide"     div
    ]

numPrims :: MapOf (NativeNum -> [Native] -> Native)
numPrims = mkMap
    [ prim0 "increment"  succ
    , prim0 "decrement"  pred
    , prim1 "add"        (+)
    , prim1 "subtract"   (-)
    , prim1 "multiply"   (*)
    , prim1 "divide"     (/)
    ]

strPrims :: MapOf (NativeStr -> [Native] -> Native)
strPrims = mkMap
    [ prim0 "length"     (size)
    , prim1 "concat"     (append)
    , prim1 "fetch"      (fetch)
    , prim2 "store"      (update)
    ]

seqPrims :: MapOf (NativeSeq -> [Native] -> Native)
seqPrims = mkMap
    [ prim0 "length"     (size)
    , prim1 "concat"     (append)
    , prim1 "fetch"      (fetch)
    , prim2 "store"      (update)
    ]

mapPrims :: MapOf (NativeMap -> [Native] -> Native)
mapPrims = mkMap
    [ prim0 "length"     (size)
    , prim0 "keys"       (indices)
    , prim0 "values"     (elems)
    , prim1 "concat"     (append)
    , prim1 "fetch"      (fetch)
    , prim2 "store"      (update)
    ]

blockPrims :: MapOf (NativeBlock -> [Native] -> Native)
blockPrims = mkMap
    [ prim0 "do"         (undefined :: NativeBlock -> Native)
    , prim1 "do_while"   (undefined :: NativeBlock -> NativeBlock -> Native)
    , prim1 "do_until"   (undefined :: NativeBlock -> NativeBlock -> Native)
    ]

prim0 :: (IsNative inv, IsNative ret)
    => String
    -> (inv -> ret)
    -> (String, inv -> [Native] -> Native)
prim0 str f = (str, f')
    where
    f' obj [] = toNative (f obj)
    f' _   _  = errArgs

prim1 :: (IsNative inv, IsNative a, IsNative ret)
    => String
    -> (inv -> a -> ret)
    -> (String, inv -> [Native] -> Native)
prim1 str f = (str, f')
    where
    f' obj [x] = toNative (f obj (fromNative x))
    f' _   _   = errArgs

prim2 :: (IsNative inv, IsNative a, IsNative b, IsNative ret)
    => String
    -> (inv -> a -> b -> ret)
    -> (String, inv -> [Native] -> Native)
prim2 str f = (str, f')
    where
    f' obj [x, y] = toNative (f obj (fromNative x) (fromNative y))
    f' _   _      = errArgs

errArgs :: forall a. a
errArgs = error "Invalid number of arguments"


