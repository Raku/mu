use v6;

=pod

Basic "if" tests.

=cut

say "1..3";

my $x = 'test';
if ($x eq $x) { say "ok 1"; } else { say "not ok 1"; }
if ($x ne $x) { say "not ok 2"; } else { say "ok 2"; }

# die called in the condition part of an if statement should die immediately
# rather than being evaluated as true
my $foo = 1;
eval 'if (die "should die") { $foo = 3 } else { $foo = 2; }';
say '# $foo = ' ~ $foo;
if ($foo == 1) { say "ok 3"; } else { say "not ok 3" }
