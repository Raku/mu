use v6;

say "1..14";

# Mostly copied from Perl 5.8.4 s t/op/bop.t

# Verify that addition/subtraction properly upgrade to doubles.
# These tests are only significant on machines with 32 bit longs,
# and two s complement negation, but should not fail anywhere.

my $a = 2147483647;
my $c=$a++;
if ($a == 2147483648) { say "ok 1" } else { say "not ok 1 # got " ~ $a }

$a = 2147483647;
$c=eval '++$a';
if ($a == 2147483648) { say "ok 2" } else { say "not ok 2 # TODO preinc" }

$a = 2147483647;
$a=$a+1;
if ($a == 2147483648) { say "ok 3" } else { say "not ok 3 # got " ~ $a }

$a = -2147483648;
$c=$a--;
if ($a == -2147483649) { say "ok 4" } else { say "not ok 4 # got " ~ $a }

$a = -2147483648;
$c=eval '--$a';
if ($a == -2147483649) { say "ok 5" } else { say "not ok 5 # TODO predec" }

$a = -2147483648;
$a=$a-1;
if ($a == -2147483649) { say "ok 6" } else { say "not ok 6 # got " ~ $a }

$a = 2147483648;
$a = -$a;
$c=$a--;
if ($a == -2147483649) { say "ok 7" } else { say "not ok 7 # got " ~ $a }

$a = 2147483648;
$a = -$a;
$c=eval '--$a';
if ($a == -2147483649) { say "ok 8" } else { say "not ok 8 # TODO predec" }

$a = 2147483648;
$a = -$a;
$a=$a-1;
if ($a == -2147483649) { say "ok 9" } else { say "not ok 9 # got " ~ $a }

$a = 2147483648;
my $b = -$a;
$c=$b--;
if ($b == -$a-1) { say "ok 10" } else { say "not ok 10" }
# $b is rightfully -2147483649, but -$a-1 is -2147483647

$a = 2147483648;
$b = -$a;
$c=eval '--$b';
if ($b == -$a-1) { say "ok 11" } else { say "not ok 11 # TODO predec" }

$a = 2147483648;
$b = -$a;
$b=$b-1;
if ($b == eval '-(++$a)') { say "ok 12" } else { say "not ok 12 # TODO preinc" }

$a = undef;
if ($a++ eq '0') { say "ok 13" } else { say "not ok 13" }

$a = undef;
if (eval '!defined($a--)') { say "ok 14" } else { say "not ok 14 # TODO defined" }

