
use v6;

role Map;

multi method delete (: $index ) returns Object;
multi method exists (: $index ) returns Object;

multi sub keys   (@array : Any|Junction *@indextests) returns Int|List
multi sub kv     (@array : Any|Junction *@indextests) returns Int|List
multi sub pairs  (@array : Any|Junction *@indextests) returns Int|(List of Pair)
multi sub values (@array : Any|Junction *@indextests) returns Int|List

1;
