#!/usr/bin/pugs

use v6;
require Test;

plan 49;

=kwid

This tests the smartmatch operator, defined in L<S04/"Smart matching">

note that ~~ is currently a stub, and is really eq.
the reason it's parsed is so that eval '' won't be around everywhere, not for
emulation.

=cut

{ #L<<S04/"Smart matching" /\QAny     Code<$>   scalar sub truth         match if $x($_)/>>
	sub uhuh { 1 }
	sub nuhuh { undef }

	todo_ok((undef ~~ \&uhuh), "scalar sub truth");
	ok(!(undef ~~ \&nuhuh), "negated scalar sub false");
};


my %hash1 = ( "foo", "Bar", "blah", "ding");
my %hash2 = ( "foo", "zzz", "blah", "frbz");
my %hash3 = ( "oink", "da", "blah", "zork");
my %hash4 = ( "bink", "yum", "gorch", "zorba");
my %hash5 = ( "foo", 1, "bar", 1, "gorch", undef, "baz", undef );

{ #L<<S04/"Smart matching" /\QHash    Hash      hash keys identical      match if $_.keys.sort »eq« $x.keys.sort/>>
	todo_ok(eval '(%hash1 ~~ %hash2)', "hash keys identical");
	ok(eval '!(%hash1 ~~ %hash4)', "hash keys differ");
};

{ #L<<S04/"Smart matching" /\QHash    any(Hash) hash key intersection    match if $_{any(Hash.keys)}/>>
	todo_ok((%hash1 ~~ any(%hash3)), "intersecting keys");
	ok(!(%hash1 ~~ any(%hash4)), "no intersecting keys");
};

{ #L<<S04/"Smart matching" /\QHash    Array     hash value slice truth   match if $_{any(@$x)}/>>
	my @true = (<foo bar>);
	my @sort_of = (<foo gorch>);
	my @false = (<gorch baz>);
	todo_ok((%hash5 ~~ @true), "value slice true");
	todo_ok((%hash5 ~~ @sort_of), "value slice partly true");
	ok(!(%hash5 ~~ @false), "value slice false");
};

{ #L<<S04/"Smart matching" /\QHash    any(list) hash key slice existence match if exists $_{any(list)}/>>
	todo_ok((%hash1 ~~ any(<foo bar>)), "any key exists (but where is it?)");
	ok(!(%hash1 ~~ any(<gorch ding>)), "no listed key exists");
};

{ #L<<S04/"Smart matching" /\QHash    all(list) hash key slice existence match if exists $_{all(list)}/>>
	todo_ok((%hash1 ~~ all(<foo blah>)), "all keys exist");
	ok(!(%hash1 ~~ all(<foo edward>)), "not all keys exist");
};

#Hash    Rule      hash key grep            match if any($_.keys) ~~ /$x/

{ #L<<S04/"Smart matching" /\QHash    Any       hash entry existence     match if exists $_{$x}/>>
	todo_ok((%hash5 ~~ "foo"), "foo exists");
	todo_ok((%hash5 ~~ "gorch"), "gorch exists, true although value is false");
	todo_ok((%hash5 ~~ "wasabi"), "wasabi does not exist");
};

{ #L<<S04/"Smart matching" /\QHash    .{Any}    hash element truth*      match if $_{Any}/>>
	my $string = "foo";
	todo_ok(eval '(%hash5 ~~ .{$string})', 'hash.{Any} truth');
	$string = "gorch";
	todo_ok(eval '!(%hash5 ~~ .{$string})', 'hash.{Any} untruth');
};

{ #L<<S04/"Smart matching" /\QHash    .<string> hash element truth*      match if $_<string>/>>
	todo_ok(eval '(%hash5 ~~ .<foo>)', "hash<string> truth");
	todo_ok(eval '!(%hash5 ~~ .<gorch>)', "hash<string> untruth");
};

{ #L<<S04/"Smart matching" /\QArray   Array     arrays are identical     match if $_ »~~« $x/>>
	ok((("blah", "blah") ~~ ("blah", "blah")), "qw/blah blah/ .eq");
	ok(!((1, 2) ~~ (1, 1)), "1 2 !~ 1 1");
};

{ #L<<S04/"Smart matching" /\QArray   any(list) list intersection        match if any(@$_) ~~ any(list)/>>
	todo_ok(((1, 2) ~~ any(2, 3)), "there is intersection between (1, 2) and (2, 3)");
	ok(!((1, 2) ~~ any(3, 4)), "but none between (1, 2) and (3, 4)");
};

{ #L<<S04/"Smart matching" /\QArray   Rule      array grep               match if any(@$_) ~~ /$x//>>
};

{ #L<<S04/"Smart matching" /\QArray   Num       array contains number    match if any($_) == $x/>>
	todo_ok(((1, 2) ~~ 1), "(1, 2) contains 1");
	ok(!((3, 4, 5) ~~ 2), "(3, 4, 5) doesn't contain 2");
};

{ #L<<S04/"Smart matching" /\QArray   Str       array contains string    match if any($_) eq $x/>>
	todo_ok((("foo", "bar", "gorch") ~~ "bar"), "bar is in qw/foo bar gorch/");
	ok(!(("x", "y", "z") ~~ "a"), "a is not in qw/x y z/");
};

{ #L<<S04/"Smart matching" /\QArray   .[number] array element truth*     match if $_[number]/>>
	todo_ok(eval '((undef, 1, undef) ~~ .[1])', "element 1 of (undef, 1, undef) is true");
	todo_ok(eval '!((undef, undef) ~~ .[0])', "element 0 of (undef, undef) is false");
};

{ #L<<S04/"Smart matching" /\QNum     NumRange  in numeric range         match if $min <= $_ <= $max/>>
	todo_ok((5 ~~ 1 .. 10), "5 is in 1 .. 10");
	ok(!(10 ~~ 1 .. 5), "10 is not in 1 .. 5");
	ok(!(1 ~~ 5 .. 10), "1 is not i n 5 .. 10");
	#ok(!(5 ~~ 5 ^..^ 10), "5 is not in 5 .. 10, exclusive"); # phooey
};

#Str     StrRange  in string range          match if $min le $_ le $max

{ #L<<S04/"Smart matching" /\QAny     Code<>    simple closure truth*    match if $x() (ignoring $_)/>>
	todo_ok((1 ~~ { 1 }), "closure truth");
	todo_ok((undef ~~ { 1 }), 'ignores $_');
};

{ #L<<S04/"Smart matching" /\QAny     Class     class membership         match if $_.does($x)/>>
	eval 'class Dog {}';
	eval 'class Cat {}';
	eval 'class Chiwawa is Dog {}'; # i'm afraid class Pugs will get in the way ;-)

	todo_ok(eval '(Chiwawa ~~ Dog)', "chiwawa isa dog");
	todo_ok(eval '!(Chiwawa ~~ Cat)', "chiwawa is not a cat");
};

#Any     Role      role playing             match if $_.does($x)

{ #L<<S04/"Smart matching" /\QAny     Num       numeric equality         match if $_ == $x/>>
	ok((1 ~~ 1), "one is one");
	ok(!(2 ~~ 1), "two is not one");
};

{ #L<<S04/"Smart matching" /\QAny     Str       string equality          match if $_ eq $x/>>
	ok(("foo" ~~ "foo"), "foo eq foo");
	ok(!("bar" ~~ "foo"), "!(bar eq foo)");
};

# no objects, no rules
# ... staring vin diesel and kevin kostner! (blech)
#Any     .method   method truth*            match if $_.method
#Any     Rule      pattern match            match if $_ ~~ /$x/
#Any     subst     substitution match*      match if $_ ~~ subst

# i don't understand this one
#Any     boolean   simple expression truth* match if true given $_

{ #L<<S04/"Smart matching" /\QAny     undef     undefined                match unless defined $_/>>
	ok(!("foo" ~~ undef), "foo is not ~~ undef");
	ok((undef ~~ undef), "undef is");
};

# does this imply MMD for $_, $x?
#Any     Any       run-time dispatch        match if infix:<~~>($_, $x)


{ #L<<S04/"Smart matching" /\Q representational checks for !~, rely on ~~ semantics to be correct, assume negated results/>>
	ok(!("foo" !~ "foo"), "!(foo ne foo)");
	ok(("bar" !~ "foo"), "bar ne foo)");

	todo_ok(!((1, 2) !~ 1), "(1, 2) contains 1");
	ok(((3, 4, 5) !~ 2), "(3, 4, 5) doesn't contain 2");

	todo_ok(!(%hash1 !~ any(%hash3)), "intersecting keys");
	ok((%hash1 !~ any(%hash4)), "no intersecting keys");
};

