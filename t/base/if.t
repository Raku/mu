use v6;

=pod

Basic "if" tests.

=cut

say "1..2";

my $x = 'test';
if ($x eq $x) { say "ok 1"; } else { say "not ok 1"; }
if ($x ne $x) { say "not ok 2"; } else { say "ok 2"; }
