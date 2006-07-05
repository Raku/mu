use v6-pugs;

use Test;

=pod

Currently, pugs treats ~~ as smartmatch, but !~ as negated string match.
This leads to craziness like the following:

    pugs> not(0 !~ rx:Perl5/0/)
    Bool::False
    pugs> not(0 ~~ rx:Perl5/0/)
    Bool::False

=cut

plan 1;

my $opposites = (not(0 ~~ rx:Perl5/0/) xor not(0 !~ rx:Perl5/0/));
ok($opposites, "~~ and !~ are opposites");
