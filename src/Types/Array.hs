module Types.Array where

import {-# SOURCE #-} AST
import Internals

type Index = Int

class ArrayClass a where
    fetch       :: a -> Index -> Eval Val
    store       :: a -> Index -> Val -> Eval ()
    fetchSize   :: a -> Eval Index
    storeSize   :: a -> Index -> Eval ()
    storeSize av sz = do
        size <- fetchSize av
        case size `compare` sz of
            GT -> mapM_ (const $ pop av) [size .. sz-1]
            EQ -> return () -- no need to do anything
            LT -> mapM_ (\idx -> store av idx undef) [size .. sz-1]
    extend      :: a -> Index -> Eval ()
    extend _ _ = return ()
    exists      :: a -> Index -> Eval VBool
    exists av idx = do
        size <- fetchSize av
        return $ size > idx
    delete      :: a -> Index -> Eval ()
    delete av idx = do
        size <- fetchSize av
        case (size - 1) `compare` idx of
            GT -> return ()                 -- no such index
            EQ -> storeSize av (size - 1)   -- truncate
            LT -> store av idx undef       -- set to undef
    clear       :: a -> Eval ()
    clear av = storeSize av 0
    push        :: a -> [Val] -> Eval ()
    push av vals = do
        size <- fetchSize av
        mapM_ (uncurry $ store av) $ [size..] `zip` vals
    pop         :: a -> Eval Val
    pop av = do
        size <- fetchSize av
        if size == 0
            then return undef
            else do
                val <- fetch av $ size - 1
                storeSize av $ size - 1
                return val
    shift       :: a -> Eval Val
    shift av = do
        vals <- splice av 0 1 []
        return $ last (undef:vals)
    unshift     :: a -> [Val] -> Eval ()
    unshift av vals = do
        splice av 0 0 vals
        return ()
    splice      :: a -> Index -> Index -> [Val] -> Eval [Val]
    splice av off len vals = callCC $ \esc -> do
        size <- fetchSize av
        if (off < 0)
            then esc =<< splice av (off + size) len vals
            else return [] 
        if (len < 0)
            then esc =<< splice av off (len + size - off) vals
            else return []
        -- ... unfinished ...    
{-
    my @result;
    for (my $i = 0; $i < $len; $i++) {
        push(@result,$obj->FETCH($off+$i));
    }
    $off = $sz if $off > $sz;
    $len -= $off + $len - $sz if $off + $len > $sz;
    if (@_ > $len) {
        # Move items up to make room
        my $d = @_ - $len;
        my $e = $off+$len;
        $obj->EXTEND($sz+$d);
        for (my $i=$sz-1; $i >= $e; $i--) {
            my $val = $obj->FETCH($i);
            $obj->STORE($i+$d,$val);
        }
    }
    elsif (@_ < $len) {
        # Move items down to close the gap
        my $d = $len - @_;
        my $e = $off+$len;
        for (my $i=$off+$len; $i < $sz; $i++) {
            my $val = $obj->FETCH($i);
            $obj->STORE($i-$d,$val);
        }
        $obj->STORESIZE($sz-$d);
    }
    for (my $i=0; $i < @_; $i++) {
        $obj->STORE($off+$i,$_[$i]);
    }
    return wantarray ? @result : pop @result;
-}
