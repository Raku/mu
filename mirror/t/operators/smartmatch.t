#!/usr/bin/pugs

use v6;
use Test;

plan 49;

=kwid

This tests the smartmatch operator, defined in L<S04/"Smart matching">

note that ~~ is currently a stub, and is really eq.
the reason it's parsed is so that eval '' won't be around everywhere, not for
emulation.

=cut

{ #L<<S04/"Smart matching" /\QAny\s+Code<\$>\s+scalar\s+sub\s+truth\s+match\s+if\s+\$x($_)/>>
	sub uhuh { 1 }
	sub nuhuh { undef }

	ok((undef ~~ &uhuh), "scalar sub truth", :todo);
	ok(!(undef ~~ &nuhuh), "negated scalar sub false");
};


my %hash1 = ( "foo", "Bar", "blah", "ding");
my %hash2 = ( "foo", "zzz", "blah", "frbz");
my %hash3 = ( "oink", "da", "blah", "zork");
my %hash4 = ( "bink", "yum", "gorch", "zorba");
my %hash5 = ( "foo", 1, "bar", 1, "gorch", undef, "baz", undef );

{ #L<<S04/"Smart matching" /\QHash\s+Hash\s+hash\s+keys\s+identical\s+match\s+if\s+\$_.keys.sort\s+»eq«\s+\$x.keys.sort/>>
	ok(eval('(%hash1 ~~ %hash2)'), "hash keys identical", :todo);
	ok(eval('!(%hash1 ~~ %hash4)'), "hash keys differ");
};

{ #L<<S04/"Smart matching" /\QHash\s+any(Hash)\s+hash\s+key\s+intersection\s+match\s+if\s+\$_{any(Hash.keys)}/>>
	ok((%hash1 ~~ any(%hash3)), "intersecting keys", :todo);
	ok(!(%hash1 ~~ any(%hash4)), "no intersecting keys");
};

{ #L<<S04/"Smart matching" /\QHash\s+Array\s+hash\s+value\s+slice\s+truth\s+match\s+if\s+\$_{any(@$x)}/>>
	my @true = (<foo bar>);
	my @sort_of = (<foo gorch>);
	my @false = (<gorch baz>);
	ok((%hash5 ~~ @true), "value slice true", :todo);
	ok((%hash5 ~~ @sort_of), "value slice partly true", :todo);
	ok(!(%hash5 ~~ @false), "value slice false");
};

{ #L<<S04/"Smart matching" /\QHash\s+any(list)\s+hash\s+key\s+slice\s+existence\s+match\s+if\s+exists\s+\$_{any(list)}/>>
	ok((%hash1 ~~ any(<foo bar>)), "any key exists (but where is it?)", :todo);
	ok(!(%hash1 ~~ any(<gorch ding>)), "no listed key exists");
};

{ #L<<S04/"Smart matching" /\QHash\s+all(list)\s+hash\s+key\s+slice\s+existence\s+match\s+if\s+exists\s+\$_{all(list)}/>>
	ok((%hash1 ~~ all(<foo blah>)), "all keys exist", :todo);
	ok(!(%hash1 ~~ all(<foo edward>)), "not all keys exist");
};

#Hash    Rule      hash key grep            match if any($_.keys) ~~ /$x/

{ #L<<S04/"Smart matching" /\QHash\s+Any\s+hash\s+entry\s+existence\s+match\s+if\s+exists\s+\$_{$x}/>>
	ok((%hash5 ~~ "foo"), "foo exists", :todo);
	ok((%hash5 ~~ "gorch"), "gorch exists, true although value is false", :todo);
	ok((%hash5 ~~ "wasabi"), "wasabi does not exist", :todo);
};

{ #L<<S04/"Smart matching" /\QHash\s+.{Any}\s+hash\s+element\s+truth*\s+match\s+if\s+\$_{Any}/s>>
	my $string = "foo";
	ok(eval('(%hash5 ~~ .{$string})'), 'hash.{Any} truth', :todo);
	$string = "gorch";
	ok(eval('!(%hash5 ~~ .{$string})'), 'hash.{Any} untruth', :todo);
};

{ #L<<S04/"Smart matching" /\QHash\s+.<string>\s+hash\s+element\s+truth*\s+match\s+if\s+\$_<string>/>>
	ok(eval('(%hash5 ~~ .<foo>)'), "hash<string> truth", :todo);
	ok(eval('!(%hash5 ~~ .<gorch>)'), "hash<string> untruth", :todo);
};

{ #L<<S04/"Smart matching" /\QArray\s+Array\s+arrays\s+are\s+identical\s+match\s+if\s+\$_\s+»~~«\s+\$x/>>
	ok((("blah", "blah") ~~ ("blah", "blah")), "qw/blah blah/ .eq");
	ok(!((1, 2) ~~ (1, 1)), "1 2 !~ 1 1");
};

{ #L<<S04/"Smart matching" /\QArray\s+any(list)\s+list\s+intersection\s+match\s+if\s+any(\@\$_)\s+~~\s+any(list)/>>
	ok(((1, 2) ~~ any(2, 3)), "there is intersection between (1, 2) and (2, 3)", :todo);
	ok(!((1, 2) ~~ any(3, 4)), "but none between (1, 2) and (3, 4)");
};

# Array   Rule      array grep               match if any(@$_) ~~ /$x/

{ #L<<S04/"Smart matching" /\QArray\s+Num\s+array\s+contains\s+number\s+match\s+if\s+any(\$_)\s+==\s+\$x/>>
	ok(((1, 2) ~~ 1), "(1, 2) contains 1", :todo);
	ok(!((3, 4, 5) ~~ 2), "(3, 4, 5) doesn't contain 2");
};

{ #L<<S04/"Smart matching" /\QArray\s+Str\s+array\s+contains\s+string\s+match\s+if\s+any(\$_)\s+eq\s+\$x/>>
	ok((("foo", "bar", "gorch") ~~ "bar"), "bar is in qw/foo bar gorch/", :todo);
	ok(!(("x", "y", "z") ~~ "a"), "a is not in qw/x y z/");
};

{ #L<<S04/"Smart matching" /\QArray\s+.[number]\s+array\s+element\s+truth*\s+match\s+if\s+\$_[number]/>>
	ok(eval('((undef, 1, undef) ~~ .[1])'), "element 1 of (undef, 1, undef) is true", :todo);
	ok(eval('!((undef, undef) ~~ .[0])'), "element 0 of (undef, undef) is false", :todo);
};

{ #L<<S04/"Smart matching" /\QNum\s+NumRange\s+in\s+numeric\s+range\s+match\s+if\s+\$min\s+<=\s+\$_\s+<=\s+\$max/>>
	ok((5 ~~ 1 .. 10), "5 is in 1 .. 10", :todo);
	ok(!(10 ~~ 1 .. 5), "10 is not in 1 .. 5");
	ok(!(1 ~~ 5 .. 10), "1 is not i n 5 .. 10");
	#ok(!(5 ~~ 5 ^..^ 10), "5 is not in 5 .. 10, exclusive"); # phooey
};

#Str     StrRange  in string range          match if $min le $_ le $max

{ #L<<S04/"Smart matching" /\QAny\s+Code<>\s+simple\s+closure\s+truth*\s+match\s+if\s+\$x()\s+(ignoring\s+\$_)/>>
	ok((1 ~~ { 1 }), "closure truth", :todo);
	ok((undef ~~ { 1 }), 'ignores $_', :todo);
};

{ #L<<S04/"Smart matching" /\QAny\s+Class\s+class\s+membership\s+match\s+if\s+\$_.does(\$x)/>>
	eval 'class Dog {}';
	eval 'class Cat {}';
	eval 'class Chiwawa is Dog {}'; # i'm afraid class Pugs will get in the way ;-)

	ok(eval('(Chiwawa ~~ Dog)'), "chiwawa isa dog", :todo);
	ok(eval('!(Chiwawa ~~ Cat)'), "chiwawa is not a cat", :todo);
};

#Any     Role      role playing             match if \$_.does(\$x)

{ #L<<S04/"Smart matching" /\QAny\s+Num\s+numeric\s+equality\s+match\s+if\s+\$_\s+==\s+\$x/>>
	ok((1 ~~ 1), "one is one");
	ok(!(2 ~~ 1), "two is not one");
};

{ #L<<S04/"Smart matching" /\QAny\s+Str\s+string\s+equality\s+match\s+if\s+\$_\s+eq\s+\$x/>>
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

{ #L<<S04/"Smart matching" /\QAny\s+undef\s+undefined\s+match\s+unless\s+defined\s+\$_/>>
	ok(!("foo" ~~ undef), "foo is not ~~ undef");
	ok((undef ~~ undef), "undef is");
};

# does this imply MMD for $_, $x?
#Any     Any       run-time dispatch        match if infix:<~~>($_, $x)


{ #L<S04/"Smart matching">
  #representational checks for !~, rely on ~~ semantics to be correct, assume negated results

	ok(!("foo" !~ "foo"), "!(foo ne foo)");
	ok(("bar" !~ "foo"), "bar ne foo)");

	ok(!((1, 2) !~ 1), "(1, 2) contains 1", :todo);
	ok(((3, 4, 5) !~ 2), "(3, 4, 5) doesn't contain 2");

	ok(!(%hash1 !~ any(%hash3)), "intersecting keys", :todo);
	ok((%hash1 !~ any(%hash4)), "no intersecting keys");
};

