#!/usr/bin/pugs

use v6;
require Test;

plan 21;

# The comments are from the synopsis 04 table

# note that ~~ is currently a stub, and is really eq.
# the reason it's parsed is so that eval '' won't be around everywhere, not for
# emulation.

#Any     Code<$>   scalar sub truth         match if $x($_)
#Hash    Hash      hash keys identical      match if $_.keys.sort »eq« $x.keys.sort
#Hash    any(Hash) hash key intersection    match if $_{any(Hash.keys)}
#Hash    Array     hash value slice truth   match if $_{any(@$x)}
#Hash    any(list) hash key slice existence match if exists $_{any(list)}
#Hash    all(list) hash key slice existence match if exists $_{all(list)}
#Hash    Rule      hash key grep            match if any($_.keys) ~~ /$x/
#Hash    Any       hash entry existence     match if exists $_{$x}
#Hash    .{Any}    hash element truth*      match if $_{Any}
#Hash    .<string> hash element truth*      match if $_<string>

{ #Array   Array     arrays are identical     match if $_ »~~« $x
	ok((("blah", "blah") ~~ ("blah", "blah")), "qw/blah blah/ .eq");
	ok(!((1, 2) ~~ (1, 1)), "1 2 !~ 1 1");
};

{ #Array   any(list) list intersection        match if any(@$_) ~~ any(list)
	todo_ok(((1, 2) ~~ any(2, 3)), "there is intersection between (1, 2) and (2, 3)");
	ok(!((1, 2) ~~ any(3, 4)), "but none between (1, 2) and (3, 4)");
};

{ #Array   Rule      array grep               match if any(@$_) ~~ /$x/
};

{ #Array   Num       array contains number    match if any($_) == $x
	todo_ok(((1, 2) ~~ 1), "(1, 2) contains 1");
	ok(!((3, 4, 5) ~~ 2), "(3, 4, 5) doesn't contain 2");
};

{ #Array   Str       array contains string    match if any($_) eq $x
	todo_ok((("foo", "bar", "gorch") ~~ "bar"), "bar is in qw/foo bar gorch/");
	ok(!(("x", "y", "z") ~~ "a"), "a is not in qw/x y z/");
};

{ #Array   .[number] array element truth*     match if $_[number]
	todo_ok(eval '((undef, 1, undef) ~~ .[1])', "element 1 of (undef, 1, undef) is true");
	todo_ok(eval '!((undef, undef) ~~ .[0])', "element 0 of (undef, undef) is false");
};

{ #Num     NumRange  in numeric range         match if $min <= $_ <= $max
	todo_ok((5 ~~ 1 .. 10), "5 is in 1 .. 10");
	ok(!(10 ~~ 1 .. 5), "10 is not in 1 .. 5");
	ok(!(1 ~~ 5 .. 10), "1 is not i n 5 .. 10");
	#ok(!(5 ~~ 5 ^..^ 10), "5 is not in 5 .. 10, exclusive"); # phooey
};

#Str     StrRange  in string range          match if $min le $_ le $max

{ #Any     Code<>    simple closure truth*    match if $x() (ignoring $_)
	todo_ok((1 ~~ { 1 }), "closure truth");
	todo_ok((undef ~~ { 1 }), 'ignores $_');
};

{ #Any     Class     class membership         match if $_.does($x)
	eval 'class Dog {}';
	eval 'class Cat {}';
	eval 'class Chiwawa is Dog {}'; # i'm afraid class Pugs will get in the way ;-)

	#todo_ok((Chiwawa ~~ Dog), "chiwawa isa dog");
	#todo_ok(!(Chiwawa ~~ Cat), "chiwawa is not a cat");
};

#Any     Role      role playing             match if $_.does($x)

{ #Any     Num       numeric equality         match if $_ == $x
	ok((1 ~~ 1), "one is one");
	ok(!(2 ~~ 1), "two is not one");
};

{ #Any     Str       string equality          match if $_ eq $x
	ok(("foo" ~~ "foo"), "foo eq foo");
	ok(!("bar" ~~ "foo"), "bar ne foo");
};

# no objects, no rules
#Any     .method   method truth*            match if $_.method
#Any     Rule      pattern match            match if $_ ~~ /$x/
#Any     subst     substitution match*      match if $_ ~~ subst

# i don't understand this one
#Any     boolean   simple expression truth* match if true given $_

{ #Any     undef     undefined                match unless defined $_
	ok(!("foo" ~~ undef), "foo is not ~~ undef");
	ok((undef ~~ undef), "undef is");
};

# does this imply MMD for $_, $x?
#Any     Any       run-time dispatch        match if infix:<~~>($_, $x)

