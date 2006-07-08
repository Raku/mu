use v6-alpha;

use Test;

=pod

Prelude.pm defines
multi sub infix:<~~> ($x, Rul $r) is primitive is safe is builtin {$r.f.($x)}
But it doesn't work.
If you eval it however, it does.

This problem is currently blocking the rules development path which
involves creating a class (Rul), which remembers the pattern, and
serves as a center point of pattern parsing, analysis, and engine
implementations.

=cut

plan 4;
skip_rest '"Rul" builtin support is now not part of Prelude anymore';
exit;

my $r = Rul.new(:f( sub ($s) { 3 } ));

is($r.f.("a"), 3, "Rul works when called directly");

ok(("a" ~~ $r),"Call via ~~ defined in Prelude.pm works. If ok, the bug is fixed!");

ok(eval('multi sub infix:<~~> ($x, Rul $r) is primitive is safe is builtin {$r.f.($x)};1'), "Evaled again the same ~~ as is defined in Prelude.pm");

ok(("a" ~~ $r),"Now calling via ~~ works");
