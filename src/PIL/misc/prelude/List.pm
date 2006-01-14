

# copied from S29draft.pod 8651.
multi  List::grep (MatchTest $test :   *@values --> Lazy ) {
    gather {
	for @values -> $x {
	    take $x if $x ~~ $test;
	}
    }
}


# copied from S29draft.pod 8651.
multi  List::join (Str $delimiter : *@values --> Str ) {
    my $str = ~@values[0];
    for 1..@values.end {
	$str ~= $delimiter ~ @values[$_];
    }
    $str;
}
# copied from S29draft.pod 8651.
&join := &join.assuming:delimiter(' ');


# copied from S29draft.pod 8651.
multi  List::map (Code $expression : *@values --> Lazy ) {
    gather {
	while @values {
	    take $expression
		.( splice(@values, 0, $expression.arity) );
	}
    }
}


# copied from S29draft.pod 8651.
multi  List::reduce (Code $expression : *@values --> Scalar ) {
    my $res;
    for @values -> $cur {
        FIRST {$res = $cur; next;}
	$res = &$expression($res, $cur);
    }
    $res;
}


# copied from S29draft.pod 8651.
multi  List::reverse ( *@values --> Lazy|Str) {
    given want {
	when List {
	    gather {
		1 while take pop @values;
	    }
	}
	when Scalar {
	    reverse @values ==> join;
	}
    }
}


# copied from S29draft.pod 8651.
type KeyExtractor  ::= Code(Any --> Any );
type Comparator    ::= Code(Any, Any --> Int );
type SortCriterion ::= KeyExtractor
                     | Comparator
                     | Pair(KeyExtractor, Comparator);

# signature from S29draft.pod 8651.
 multi Array::sort(                 @values is rw,
                                              *&by
                              :           Bit :$inplace
                             --> Array )
{...}
# signature from S29draft.pod 8651.
 multi Array::sort(                 @values is rw,
                                SortCriterion  @by
                              :           Bit :$inplace
                             --> Array )
{...}
# signature from S29draft.pod 8651.
 multi Array::sort(                 @values is rw
                              : SortCriterion :$by = &infix:<cmp>,
                                          Bit :$inplace
                             --> Array )
{...}
# signature from S29draft.pod 8651.
 multi  List::sort(  SortCriterion  @by
                              :               *@values
                             --> List )
{...}
# signature from S29draft.pod 8651.
 multi  List::sort( SortCriterion  $by = &infix:<cmp>,
                                              *@values
                             --> List )
{...}


# signature from PIL2JS Array.pm and S29draft.pod's List::zip. r8593.
# But Prim.hs is \\n   List      list    Y       safe   (Array)\
#   ie, both return and argument types differ.
multi infix:«Y» (Array *@arrays --> Lazy) { zip *@arrays }
# signature from PIL2JS Array.pm and S29draft.pod's List::zip. r8593.
# But Prim.hs is \\n   List      list    ¥       safe   (Array)\
#   ie, both return and argument types differ.
multi infix:«¥» (Array *@arrays --> Lazy) { zip *@arrays }

# copied from S29draft.pod 8651.
multi List::zip ( Array *@lists, Bit :$shortest --> Lazy ) {
    gather {
	while $shortest ?? all(@lists) !! any(@lists) {
	    for @lists -> @list {
		take shift @list;
	    }
	}
    }
}
## tweaked from PIL2JS Array.pm r8593.
#multi List::zip (Array *@arrays --> List) {
#  my $maxlen = max map { +$_ } @arrays;
#  map {
#    my $i := $_;
#    map { @arrays[$_][$i] } 0..@arrays.end;
#  } 0..$maxlen-1;
#}


# derived from PIL2JS Array.pm r8593.
# not in S29draft.pod.
# Prim.hs line is \\n   Scalar    pre     min     safe   (List)\
multi List::min(Code $cmp = &infix:«<=>», *@array --> Scalar) {
  # Hack, see comment at &sort.
  unless $cmp.isa("Code") {
    unshift @array, @$cmp;
    $cmp := &infix:«<=>»;
  }
  @array.max:{ $cmp($^b, $^a) };
}
&min ::= *&List::min;
# derived from PIL2JS Array.pm r8593.
# not in S29draft.pod.
# Prim.hs line is \\n   Scalar    pre     max     safe   (List)\
multi List::max(Code $cmp = &infix:«<=>», *@array --> Scalar) {
  # Hack, see comment at &sort.
  unless $cmp.isa("Code") {
    unshift @array, @$cmp;
    $cmp := &infix:«<=>»;
  }
  my $max = @array.shift;
  $max = ($cmp($max, $_)) < 0 ?? $_ !! $max for @array;
  $max;
}
&max ::= *&List::max;


\\n   List      pre     sort    safe   (Array)\
\\n   List      pre     sort    safe   (Array: Code)\
\\n   Int       pre     List::elems   safe   (Array)\
\\n   Int       pre     List::end     safe   (Array)\


\\n   List      pre     list    safe   (List)\
\\n   Hash      pre     hash    safe   (List)\
\\n   List      pre     pair    safe   (List)\
\\n   Any       pre     reverse safe   (List)\
\\n   List      spre    ^       safe   (Scalar)\
\\n   List      post    ...     safe   (Str)\
\\n   List      post    ...     safe   (Scalar)\
\\n   Bool      pre     not     safe   (List)\
\\n   List      pre     gather  safe   (Code)\
\\n   List      pre     map     safe   (Code, List)\
\\n   List      pre     grep    safe   (Code, List)\
\\n   List      pre     sort    safe   (Code, List)\
\\n   List      pre     reduce  safe   (Code, List)\
\\n   List      pre     sort    safe   (Array)\
\\n   List      pre     map     safe   (Array: Code)\
\\n   List      pre     grep    safe   (Array: Code)\
\\n   List      pre     sort    safe   (Array: Code)\
\\n   List      pre     reduce  safe   (Array: Code)\
\\n   Any       pre     List::splice  safe   (rw!Array, ?Int=0)\
\\n   Any       pre     List::splice  safe   (rw!Array, Int, Int)\
\\n   Any       pre     List::splice  safe   (rw!Array, Int, Int, List)\
\\n   Int       pre     push    safe   (rw!Array, List)\
\\n   Int       pre     unshift safe   (rw!Array, List)\
\\n   Scalar    pre     List::pop     safe   (rw!Array)\
\\n   Scalar    pre     List::shift   safe   (rw!Array)\
\\n   Scalar    pre     sum     safe   (List)\
\\n   Scalar    pre     min     safe   (List)\
\\n   Scalar    pre     max     safe   (List)\
\\n   List      pre     uniq    safe   (List)\
\\n   Str       pre     join    safe   (Str, List)\
\\n   List      pre     zip     safe   (List)\
\\n   List      pre     keys    safe   (rw!Hash)\
\\n   List      pre     values  safe   (rw!Hash)\
\\n   List      pre     List::kv      safe   (rw!Hash)\
\\n   List      pre     pairs   safe   (rw!Hash)\
\\n   List      pre     keys    safe   (rw!Array)\
\\n   List      pre     values  safe   (rw!Array)\
\\n   List      pre     List::kv      safe   (rw!Array)\
\\n   List      pre     pairs   safe   (rw!Array)\
\\n   Junction  pre     any     safe   (List)\
\\n   Junction  pre     all     safe   (List)\
\\n   Junction  pre     one     safe   (List)\
\\n   Junction  pre     none    safe   (List)\
\\n   Int       pre     List::elems   safe   (Array)\
\\n   Int       pre     List::end     safe   (Array)\
\\n   List      pre     keys    safe   (rw!Pair)\
\\n   List      pre     values  safe   (Pair|Junction)\
\\n   List      pre     Pair::kv      safe   (rw!Pair)\
\\n   List      pre     pairs   safe   (rw!Pair)\
\\n   List      pre     Str::split   safe   (Str)\
\\n   List      pre     Str::split   safe   (Str: Str)\
\\n   List      pre     Str::split   safe   (Str: Pugs::Internals::VRule)\
\\n   List      pre     Str::split   safe   (Str: Str, Int)\
\\n   List      pre     Str::split   safe   (Str: Pugs::Internals::VRule, Int)\
\\n   List      pre     split   safe   (Str, Str)\
\\n   List      pre     split   safe   (Str, Str, Int)\
\\n   List      pre     split   safe   (Pugs::Internals::VRule, Str)\
\\n   List      pre     split   safe   (Pugs::Internals::VRule, Str, Int)\
\\n   List      spre    =       safe   (Any)\
\\n   List      left    xx      safe   (Any, Int)\
\\n   List      non     ..      safe   (Scalar, Scalar)\
\\n   List      non     ..^     safe   (Scalar, Scalar)\
\\n   List      non     ^..     safe   (Scalar, Scalar)\
\\n   List      non     ^..^    safe   (Scalar, Scalar)\
\\n   List      spre    <==     safe   (List)\
\\n   List      left    ==>     safe   (List, Code)\
\\n   List      spre    prefix:[,]  safe   (List)\
