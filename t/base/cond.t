use v6;

=pod

Make sure conditional operators work

=cut

say "1..2";

my $x = '0';

if ($x eq $x) {say "ok 1"; } else { say "not ok 1"; }
if($x == $x) {say "ok 2"; } else { say "not ok 2"; }

