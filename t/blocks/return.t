use v6-alpha;

use Test;

plan 7;

# Is there a better reference for the spec for how return return works? 
# There is "return function" but that's a more advanced feature.
#L<S04/"Control Exceptions">

=pod

Basic tests for "return"

=cut

sub foo { return 1; }
is(foo(), 1, '... foo() returned 1 correctly');

sub bar { return }
is(bar(), undef, '... bare return statement returned undef');

sub bar2 { return() }
is(bar2(), undef, '... bare return statement w/ parens returned undef');

sub baz { return 10 if 1; }
is(baz(), 10, '... return worked with a statement modifier');

sub foobar { return if 1; };
is(foobar(), undef, '... bare return worked with a statement modifier');

sub foobar2 { return() if 1; }
is(foobar2(), undef, '... bare return worked with a statement modifier');

is( try { sub foo { my $x = 1; while $x-- { return 24; } return 42; } foo() }, 24, 'return in while');

