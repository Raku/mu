use v6-alpha;

use Test;

plan 1;

#L<S03/New operators/"A negated smart match is spelled">

my $opposites = (not(0 ~~ rx:Perl5/0/) xor not(0 !~~ rx:Perl5/0/));
ok($opposites, "~~ and !~~ are opposites");
