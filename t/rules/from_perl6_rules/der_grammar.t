#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/der_grammar.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 33;

if(!eval('("a" ~~ /a/)')) {
    skip_rest "skipped tests - rules support appears to be missing";
} else {

grammar Other {
    rule abc { a (<?bee>) c }

    rule bee { b }

    rule def { d <eh> f }

    rule eh  { e }
}

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

fail("FIXME parsefail", :todo);
#ok(eval(q{'abc' ~~ m/^ (<Yet::Another.abc>) $/ }), '<Yet::Another.abc>', :todo<feature>);
is($/, "abc", 'abc $/', :todo<feature>);
is($0, "abc", 'abc $0', :todo<feature>);

fail("FIXME parsefail", :todo);
#ok(eval(q{!( 'abc' ~~ m/ (<Yet::Another.bee>) / ) }), 'abc <Yet::Another.bee>');
fail("FIXME parsefail", :todo);
#ok(eval(q{'aBc' ~~ m/ (<Yet::Another.bee>) / }), 'aBc <Yet::Another.bee>', :todo<feature>);
is($/, "B", 'Yet::Another::bee $/', :todo<feature>);
is($0, "B", 'Yet::Another::bee $0', :todo<feature>);

fail("FIXME parsefail", :todo);
#ok(eval(q{!( 'def' ~~ m/^ (<Yet::Another.def>) $/ ) }), 'def (<Yet::Another.def>)');
fail("FIXME parsefail", :todo);
#ok(eval(q{'DeF' ~~ m/^ (<Yet::Another.def>) $/ }), 'DeF (<Yet::Another.def>)', :todo<feature>);
is($/, "DeF", 'DeF $/', :todo<feature>);
is($0, "DeF", 'DeF $0', :todo<feature>);

fail("FIXME parsefail", :todo);
#ok('DeF' ~~ m/^ <?Yet::Another.def> $/, '<?Yet::Another.def>', :todo<feature>);
is($/, "DeF", '?Yet::Another.def $/', :todo<feature>);
ok($0 ne "DeF", '?Yet::Another.def $0');
is($/<def>, "DeF", '?def $/<def>', :todo<feature>);
is(eval('$/<def><eh>'), "e", '?def $/<def><eh>', :todo<feature>);


# Non-existent rules...

fail("FIXME parsefail", :todo);
#ok(!eval(q{ 'abc' ~~ m/ (<Another.sea>) /  }), '<Another.sea>');
is($!, 'Error', :todo<feature>);

}

