use v6;

# again, from Perl 5

say "1..37";

my $x = 10000;
if (0 + ++$x - 1 == 10000) { say "ok 1" } else { say "not ok 1" }
if (0 + $x-- - 1 == 10000) { say "ok 2" } else { say "not ok 2" }
if (1 * $x == 10000)       { say "ok 3" } else { say "not ok 3" }
if (0 + $x-- - 0 == 10000) { say "ok 4" } else { say "not ok 4" }
if (1 + $x == 10000)       { say "ok 5" } else { say "not ok 5" }
if (1 + $x++ == 10000)     { say "ok 6" } else { say "not ok 6" }
if (0 + $x == 10000)       { say "ok 7" } else { say "not ok 7" }
if (0 + --$x + 1 == 10000) { say "ok 8" } else { say "not ok 8" }
if (0 + ++$x + 0 == 10000) { say "ok 9" } else { say "not ok 9" }
if ($x == 10000)           { say "ok 10" } else { say "not ok 10" }

my @x;
@x[0] = 10000;
if (0 + ++@x[0] - 1 == 10000) { say "ok 11" } else { say "not ok 11" }
if (0 + @x[0]-- - 1 == 10000) { say "ok 12" } else { say "not ok 12" }
if (1 * @x[0] == 10000)       { say "ok 13" } else { say "not ok 13" }
if (0 + @x[0]-- - 0 == 10000) { say "ok 14" } else { say "not ok 14" }
if (1 + @x[0] == 10000)       { say "ok 15" } else { say "not ok 15" }
if (1 + @x[0]++ == 10000)     { say "ok 16" } else { say "not ok 16" }
if (0 + @x[0] == 10000)       { say "ok 17" } else { say "not ok 17" }
if (0 + --@x[0] + 1 == 10000) { say "ok 18" } else { say "not ok 18" }
if (0 + ++@x[0] + 0 == 10000) { say "ok 19" } else { say "not ok 19" }
if (@x[0] == 10000)           { say "ok 20" } else { say "not ok 20" }

#my %z;
#%z{0} = 10000;
#if (0 + ++%z{0} - 1 == 10000) { say "ok 21" } else { say "not ok 21" }
#if (0 + %z{0}-- - 1 == 10000) { say "ok 22" } else { say "not ok 22" }
#if (1 * %z{0} == 10000)       { say "ok 23" } else { say "not ok 23" }
#if (0 + %z{0}-- - 0 == 10000) { say "ok 24" } else { say "not ok 24" }
#if (1 + %z{0} == 10000)       { say "ok 25" } else { say "not ok 25" }
#if (1 + %z{0}++ == 10000)     { say "ok 26" } else { say "not ok 26" }
#if (0 + %z{0} == 10000)       { say "ok 27" } else { say "not ok 27" }
#if (0 + --%z{0} + 1 == 10000) { say "ok 28" } else { say "not ok 28" }
#if (0 + ++%z{0} + 0 == 10000) { say "ok 29" } else { say "not ok 29" }
#if (%z{0} == 10000)           { say "ok 30" } else { say "not ok 30" }
for (21..30) { say "not ok " ~ $_ ~ " # TODO %z{0} (manually uncomment)"; }

# test magical autoincrement

my $foo;
$foo = '99';
if (++$foo eq '100') { say "ok 31" } else { say "not ok 31" }
$foo = 'a0';
if (++$foo eq 'a1')  { say "ok 32" } else { say "not ok 32" }
$foo = 'Az';
if (++$foo eq 'Ba')  { say "ok 33" } else { say "not ok 33" }
$foo = 'zz';
if (++$foo eq 'aaa') { say "ok 34" } else { say "not ok 34" }
$foo = 'A99';
if (++$foo eq 'B00') { say "ok 35" } else { say "not ok 35" }
# EBCDIC guards: i and j, r and s, are not contiguous.
$foo = 'zi';
if (++$foo eq 'zj')  { say "ok 36" } else { say "not ok 36" }
$foo = 'zr';
if (++$foo eq 'zs')  { say "ok 37" } else { say "not ok 37" }

