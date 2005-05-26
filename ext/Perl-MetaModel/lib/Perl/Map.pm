
use v6;

# this is a super-class for Arrays, Hashes, Sets and other collections
# like Bags, ordered hashes and so on.

role Map;

# FIXME - no insert methods
multi method delete (: $index ) returns Object;
multi method delete (: *@index ) returns List of Object;
multi method exists (: $index ) returns Object;
multi method exists (: *@index ) returns List of Bool;

multi sub keys   (@array : Any|Junction *@indextests) returns List of Object
multi sub kv     (@array : Any|Junction *@indextests) returns List of Object|Object
multi sub pairs  (@array : Any|Junction *@indextests) returns List of Pair
multi sub values (@array : Any|Junction *@indextests) returns Object|List

1;
