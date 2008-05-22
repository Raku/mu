use v6;

use Test;

plan 1;

#L<S03/Chaining binary precedence/"A negated smart match is spelled">

my $opposites = (not(0 ~~ rx:Perl5/0/) xor not(0 !~~ rx:Perl5/0/));
ok($opposites, "~~ and !~~ are opposites");
