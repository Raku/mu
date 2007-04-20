use v6-alpha;

use Test;

plan 1;

=pod

Perl 6 has an explicitly declared C<=~> which should die at compile time
and is intended to catch user "brainos"; it recommends C<~~> or C<~=> to
the user instead.

=cut

#L<S03/Chaining binary precedence/"To catch">

my $str = 'foo';
eval q{$str =~ m/bar/;};
if $!.defined {
    pass "caught =~ braino, saying $!";
}
else {
    flunk "didn't catch =~ braino";
}
