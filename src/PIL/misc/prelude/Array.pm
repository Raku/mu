# The idea is to comment out things which end up defined in PILn
# (though one may wish to overwrite a limited piln version with a more
# general p6 one after bootstrap).  Or spec code which doesnt actually
# run yet.  The immediate goal is simply to permit a piln backend to
# pass most of the tests the current backend does.


role rArray {}


class Array is List does rArray;

# signature from S29draft.pod r8593.
# XXX Prim.hs has this as (rw!Array).  Why rw?
# \\n   List      pre     keys    safe   (rw!Array)\
multi Array::keys   (@array : MatchTest *@indextests --> Int|List )
{
    my @ret;
    for @array.kv -> ($k, $v) {
	push(@ret, $k ) if $k ~~ any(@indextests);
    }
    *@ret; # want Item ?? +@ret !! *@ret;
}
# signature from S29draft.pod r8593.
# Prim.hs defines this as List::kv.  And with rw!Array.
multi Array::kv     (@array : MatchTest *@indextests --> Int|List )
{
    my @ret;
    for @array.keys -> $k {
	push(@ret, ($k,@array[$k]) ) if $k ~~ any(@indextests);
    }
    *@ret; # want Item ?? +@ret !! *@ret;
}
# signature from S29draft.pod r8593.
# XXX Prim.hs has this as (rw!Array).  Why rw?
# \\n   List      pre     pairs   safe   (rw!Array)\
# signature doesnt parse. using simplified one.
#multi Array::pairs  (@array : MatchTest *@indextests --> Int|(List of Pair) )
multi Array::pairs  (@array : MatchTest *@indextests )
{
    my @ret;
    for @array.keys -> $k {
	push(@ret, Pair($k,@array[$k]) ) if $k ~~ any(@indextests);
    }
    *@ret; # want Item ?? +@ret !! *@ret;
}
# signature from S29draft.pod r8593.
# XXX Prim.hs has this as (rw!Array).  Why rw?
# \\n   List      pre     values  safe   (rw!Array)\
multi Array::values (@array : MatchTest *@indextests --> Int|List )
{
    my @ret;
    for @array.keys -> $k {
	push(@ret, @array[$k] ) if $k ~~ any(@indextests);
    }
    *@ret; # want Item ?? +@ret !! *@ret;
}


# signature from S29draft.pod r8593.
# XXX Prim.hs has this as (rw!Array).  Why rw?
# XXX Prim.hs has a different return type, Scalar vs List.
# \\n   Scalar    pre     delete  safe   (rw!Array: List)\
multi method Array::delete (@array : *@indices --> List )
{
    my @ret;
    for @indicies -> $k {
	push(@ret, @array._delete_key($k) );
    }
    *@ret;
}
# signature from S29draft.pod r8593.
# XXX Prim.hs has this as (rw!Array).  Why rw?
# \\n   Bool      pre     exists  safe   (rw!Array: Int)\
multi method Array::exists (@array : Int *@indices --> Bool )
{
    my @ret;
    for @indicies -> $k {
	push(@ret, @array._exists_key($k) );
    }
    *@ret;
}
 

# copied from S29draft.pod r8649.
&Array::pop := &Array::splice.assuming(:offset(-1) :length(1));
## an alternate implementation:
#multi method Array::pop (@array is rw : --> Scalar) {
#    @array.splice(-1,1);
#}

# copied from S29draft.pod r8649.
# signature doesnt parse. using simplified one.
#multi Array::pop ( --> Scalar ) {
multi Array::pop (  ) {
    Array::pop @+_;
}

# copied from S29draft.pod r8593.
multi Array::push (@array is rw : *@values --> Int ) {
    Array::splice(@array, @array.elems, 0, @values);
    @array.elems;
}

# copied from S29draft.pod r8649.
&Array::shift := &Array::splice.assuming(:offset(0) :length(1));
## an alternate implementation:
#multi method Array::shift (@array is rw : --> Scalar) {
#    @array.splice(0,1);
#}

# copied from S29draft.pod r8649.
# signature doesnt parse. using simplified one.
#multi Array::shift ( --> Scalar ) {
multi Array::shift (  ) {
    Array::shift @+_;
}


# signature from S29draft.pod r8593.
# body from PIL2JS (r8593) from Prelude/PIL.pm.
# Prim.hs r8593 defines this as List::splice.
multi Array::splice (       @array is rw 
			    : Int $offset = 0,
			    Int $length,
			    *@values
			    --> List ) is rw
{
    my $off = +$offset;
    my $len = $length;
    my $size = +@array;

    $off += $size if $off < 0;
    if $off > $size {
        warn "splice() offset past end of array\n";
        $off = $size;
    }
    # $off is now ready

    $len = +$len if defined($len);
    $len = $size - $off if !defined($len);
    $len = $size + $len - $off if $len < 0;
    $len = 0 if $len < 0;
    # $len is now ready

    my $listlen = +@values;
    my $size_change = $listlen - $len;
    my @result;

    if 1 {
        my $i = $off;
        my $stop = $off + $len;
        while $i < $stop {
            push(@result,@array[$i]);
            $i++;
        }
    }

    if $size_change > 0 {
        my $i = $size + $size_change -1;
        my $final = $off + $size_change;
        while $i >= $final {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @array[$foo], see t/operators/binding/arrays.t.
            @array.delete($i);
            @array[$i] = @array[$i-$size_change];
            $i--;
        }
    } elsif $size_change < 0 {
        my $i = $off;
        my $final = $size + $size_change -1;
        while $i <= $final {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @array[$foo], see t/operators/binding/arrays.t.
            @array.delete($i);
            @array[$i] = @array[$i-$size_change];
            $i++;
        }
        # +@array = $size + $size_change;
        #   doesnt exist yet, so...
        my $n = 0;
        while $n-- > $size_change {
            pop(@array);
        }
    }

    if $listlen > 0 {
        my $i = 0;
        while $i < $listlen {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @array[$foo], see t/operators/binding/arrays.t.
            @array.delete($off+$i);
            @array[$off+$i] = @values[$i];
            $i++;
        }
    }

    #  want.List ?? *@result !! +@result ?? @result[-1] !! undef;
    @result;
}


# copied from S29draft.pod r8593.
multi Array::unshift (@array is rw : *@values --> Int ) {
    Array::splice(@array, 0, 0, @values);
    @array.elems;
}

# name from docs/notes/piln_object_repr_types.pod r8593.
# while (@array: --> List) { seems a preferable phrasing, it doesnt parse.
multi method Array::as_seq (@array:) returns List {
    *@array
}
# name from docs/notes/piln_object_repr_types.pod r8593.
multi method Array::as_map (@array:) returns Hash {
    hash(@array.pairs);
}
# name from docs/notes/piln_object_repr_types.pod r8593.
multi method Array::as_int (@array:) returns Int {
    +@array;
}
# name from docs/notes/piln_object_repr_types.pod r8593.
multi method Array::as_num (@array:) returns Num {
    +@array;
}

# speculative?
# behavior based on pugs r8593.
# while (@array: --> List) { seems a preferable phrasing, it doesnt parse.
multi method Array::as_str (@array:) returns Str {
    join(" ",@array.values);
}

# speculative?
# behavior based on pugs r8593.
# while (@array: --> List) { seems a preferable phrasing, it doesnt parse.
multi method Array::perl (@array:) returns Str {
    "["~ join(", ", @array.values.map:{$_.perl} ) ~"]";
}

# tweaked from PIL2JS Operators.pm r8593.
# Prim.hs says \\n   Scalar    left    .[]     safe   (Array, Int)\.
sub prefix:<[.[]]> (*$head is copy, *@rest is copy) {
  while @rest {
    $head = $head[shift @rest];
  }
  $head;
}


# signature from S29draft.pod 8651.  In the List section.
multi Array::grep (@values :      Code *&test   --> Lazy )
{
    gather {
	for @values -> $x {
	    take $x if &test($x);
	}
    }
}
# signature from S29draft.pod 8655.  In the List section.
multi Array::grep (@values :  MatchTest $test   --> Lazy )
{
    List::grep $test, *@values;
}

# signature from S29draft.pod 8655.  In the List section.
multi Array::join (@values :  Str $delimiter --> Str )
{
    List::join $delimiter, *@values;
}

# signature from S29draft.pod 8655.  In the List section.
multi Array::map (@values :  Code $expression --> Lazy ) 
{
    List::map $expression, *@values;
}

# signature from S29draft.pod 8651.  In the List section.
# Prim.hs has a different return type.  List instead of Scalar.
# \\n   List      pre     reduce  safe   (Array: Code)\
multi Array::reduce (@values : Code *&expression --> Scalar )
{
    List::reduce *&expression, *@values;
}

# signature from S29draft.pod 8651.  In the List section.
multi Array::reverse (   @values --> Lazy|Str) {
    List::reverse *@values;
}


# derived from PIL2JS Array.pm r8593.
# not in S29draft, but test cases exist.
multi Array::min(@self: Code $cmp = &infix:«<=>» --> Scalar) {
    List::min $cmp, *@self;
}
# derived from PIL2JS Array.pm r8593.
# not in S29draft, but test cases exist.
multi Array::max(@self: Code $cmp = &infix:«<=>» --> Scalar) {
    List::max $cmp, *@self;
}


# XXX - TODO
# postcircumfix:<[ ]>  circumfix:<[ ]>


