#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/der_grammar.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 34;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

# PGE doesn't parse non-capturing subrules.
# So the real test grammar parsefails.
# This modified grammar breaks some tests below.
my $workaround = 1;
skip "PGE doesn't parse non-capturing subrules.  Parsefails.";
#ok !$workaround,"Defining rule with non-capturing subrule without parsefail.";
if $workaround {eval '
grammar Other {
	rule abc { a (<bee>) c } # avoid parsefail

	rule bee { b }

	rule def { d <eh> f }

	rule eh  { e }
}
'} else {eval '
grammar Other {
	rule abc { a (<?bee>) c }

	rule bee { b }

	rule def { d <eh> f }

	rule eh  { e }
}
'}

grammar Another is Other {
}

grammar Yet::Another is Another {

	rule bee { B }

	rule def { D <eh> F }
}

# Test derivation and Liskov substitutability...

ok('abc' ~~ m/^ (<Another.abc>) $/, '<Another.abc>', :todo<feature>);
is($/, "abc", 'abc $/', :todo<feature>);
is($0, "abc", 'abc $0', :todo<feature>);

ok('abc' ~~ m/ (<Another.bee>) /, '<Another.bee>', :todo<feature>);
is($/, "b", 'bee $/', :todo<feature>);
is($0, "b", 'bee $0', :todo<feature>);

ok('b' ~~ m/ (<Another.bee>) /, '<Another.bee>', :todo<feature>);

ok('def' ~~ m/^ (<Another.def>) $/, '(<Another.def>)', :todo<feature>);
is($/, "def", 'def $/', :todo<feature>);
is($0, "def", 'def $0', :todo<feature>);

ok('def' ~~ m/^ <?Another.def> $/, '<?Another.def>', :todo<feature>);
is($/, "def", '?def $/', :todo<feature>);
ok($0 ne "def", '?def $0');
is($/<def>, "def", '?def $/<def>', :todo<feature>);
eval_is('$/<def><eh>', "e", '?def $/<def><eh>', :todo<feature>);


# Test rederivation and polymorphism...

ok('abc' ~~ m/^ (<Yet::Another.abc>) $/, '<Yet::Another.abc>', :todo<feature>);
is($/, "abc", 'abc $/', :todo<feature>);
is($0, "abc", 'abc $0', :todo<feature>);

ok(!( 'abc' ~~ m/ (<Yet::Another.bee>) / ), 'abc <Yet::Another.bee>');
ok('aBc' ~~ m/ (<Yet::Another.bee>) /, 'aBc <Yet::Another.bee>', :todo<feature>);
is($/, "B", 'Yet::Another::bee $/', :todo<feature>);
is($0, "B", 'Yet::Another::bee $0', :todo<feature>);

ok(!( 'def' ~~ m/^ (<Yet::Another.def>) $/ ), 'def (<Yet::Another.def>)');
ok('DeF' ~~ m/^ (<Yet::Another.def>) $/, 'DeF (<Yet::Another.def>)', :todo<feature>);
is($/, "DeF", 'DeF $/', :todo<feature>);
is($0, "DeF", 'DeF $0', :todo<feature>);

ok('DeF' ~~ m/^ <?Yet::Another.def> $/, '<?Yet::Another.def>', :todo<feature>);
is($/, "DeF", '?Yet::Another.def $/', :todo<feature>);
ok($0 ne "DeF", '?Yet::Another.def $0');
is($/<def>, "DeF", '?def $/<def>', :todo<feature>);
eval_is('$/<def><eh>', "e", '?def $/<def><eh>', :todo<feature>);


# Non-existent rules...

ok(!eval(q{ 'abc' ~~ m/ (<Another.sea>) / }), '<Another.sea>');
ok($!, 'Error', :todo<feature>);

}

