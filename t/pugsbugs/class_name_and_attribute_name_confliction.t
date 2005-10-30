#!/usr/bin/pugs

use v6;
use Test;

=pod

The parser won't do right thing when two(or more) class-es get
attributes whose name are the same.

hmm, It's conflicted with class name and attribute name.

***** These two examples below will fail to parse. *****

##### this example below will cause pugs hang.
class a {has $.a; method update { $.a; } }; class b { has $.a; submethod BUILD { a.new( a => $.a ).update; } };class c { has $.b; submethod BUILD { b.new( a => $.b ); } };c.new( b => 30 );

##### this example will say sub isn't found.
class a { has $.a; method update { $.a; } };class b { has $.a; submethod BUILD { a.new( a => $.a ).update; }; }; b.new( a => 20 );

Problems with this test:
* The classes "a", "b", and "c" are redefined several times.
  { class Foo {...} }; say Foo.new;
  # works, even though Foo was declared in a different lexical scope
  Proposal: Change the class names to "a1", "b1", "a2", "b2", etc.

* This also causes some infloops, as some classes' BUILD call itself
  (indirectly) (this is a consequence of the first problem).

* It's *sub*method BUILD, not method BUILD.

  Proposal: s/method BUILD/submethod BUILD/

* class Foo {...}; eval "Foo" doesn't resolve "Foo" to the class Foo currently
  -- one has to use eval "::Foo" or, even better, try {...}.

  Proposal: s/eval "..."/try {...}/;

=cut

plan 3;


{
    my $var = 100;
    class a {
        has $.a;
        has $.c;
        method update { $var -= $.a; }
    };
    eval 'a.new( a => 10 ).update';
    is $var, 90, "Testing suite 1.";
}



{
    my $var = 100;
    class a {
        has $.a;
        method update { $var -= $.a; }
    };
    class b {
        has $.a;
        method BUILD { a.new( a => $.a ).update; };
    };

=pod
  pugs> class a { has $.a; method update { $var -= $.a; } };class b { has $.a; submethod BUILD { a.new( a => $.a ).update; };};b.new( a => 20 );
  *** No compatible subroutine found: "&update"
      at <interactive> line 1, column 90-114
      <interactive> line 1, column 120-136
      <interactive> line 1, column 120-136
  pugs>

=cut

##### will cause pugs hang if uncomment it
#    eval 'b.new( a => 20 )';
    is $var, 80, "Testing suite 2.";
}



{
    my $var = 100;
    class a {
        has $.a;
        method update { $var -= $.a; }
    };
    class b {
        has $.a;
        method BUILD { a.new( a => $.a ); }
    };
    class c {
        has $.b;
        method BUILD { b.new( a => $.b ); }
    };

##### cause pugs hang.
#    eval 'c.new( b => 30 ).update';
    is $var, 70, "Testing suite 3.";
}

