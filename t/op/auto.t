#!/usr/bin/pugs

use v6;
require Test;

# again, from Perl 5

plan 37;

my $base = 10000;

my $x = 10000;
is(0 + ++$x - 1, $base);
is(0 + $x-- - 1, $base);
is(1 * $x,       $base);
is(0 + $x-- - 0, $base);
is(1 + $x,       $base);
is(1 + $x++,     $base);
is(0 + $x,       $base);
is(0 + --$x + 1, $base);
is(0 + ++$x + 0, $base);
is($x,           $base);

my @x;
@x[0] = 10000;
is(0 + ++@x[0] - 1, $base);
is(0 + @x[0]-- - 1, $base);
is(1 * @x[0],       $base);
is(0 + @x[0]-- - 0, $base);

# NOTE: (stevan 2/24/2005)
# these 4 tests should be able to use is()
# but there seems to be an odd bug in is()
# which I cannot figure out right now
ok(1 + @x[0] == $base);
ok(1 + @x[0]++ == $base);
ok(0 + @x[0] == $base);
ok(0 + ++@x[0] + 0 == $base);

is(0 + --@x[0] + 1, $base);
is(@x[0],           $base);

#my %z;
#%z{0} = 10000;
#ok (0 + ++%z{0} - 1 == 10000);
#ok (0 + %z{0}-- - 1 == 10000);
#ok (1 * %z{0} == 10000);
#ok (0 + %z{0}-- - 0 == 10000);
#ok (1 + %z{0} == 10000);
#ok (1 + %z{0}++ == 10000);
#ok (0 + %z{0} == 10000);
#ok (0 + --%z{0} + 1 == 10000);
#ok (0 + ++%z{0} + 0 == 10000);
#ok (%z{0} == 10000);
for (21..30) { todo_fail("%z{0} (manually uncomment)"); }

# test magical autoincrement

my $foo;

$foo = '99';
is(++$foo, '100');

$foo = 'a0';
is(++$foo, 'a1');

$foo = 'Az';
is(++$foo, 'Ba');

$foo = 'zz';
is(++$foo, 'aaa');

$foo = 'A99';
is(++$foo, 'B00');

# EBCDIC guards: i and j, r and s, are not contiguous.
$foo = 'zi';
is(++$foo, 'zj');

$foo = 'zr';
is(++$foo, 'zs');

