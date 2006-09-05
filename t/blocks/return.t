use v6-alpha;
use Test; plan 8;

# Is there a better reference for the spec for how return return works? 
# There is "return function" but that's a more advanced feature.
#L<S04/"Control Exceptions">

=pod

Basic tests for "return"

=cut

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

my $should_ret_empty_list1 = sub { return; 1 };
is @($should_ret_empty_list1(),).elems, 0, "our sub returned an empty list (1)";

sub return_1 { return 1; }
is(return_1(), 1, '... return_1() returned 1 correctly');

is( try { sub foo { my $x = 1; while $x-- { return 24; } return 42; } foo() }, 24, 'return in while');

