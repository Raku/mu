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

is(1 + @x[0], $base);
is(1 + @x[0]++, $base);
is(0 + @x[0], $base);
is(0 + ++@x[0] + 0, $base);

is(0 + --@x[0] + 1, $base);
is(@x[0],           $base);

my %z = 0, 10000;
ok (0 + ++%z{0} - 1 == 10000);
ok (0 + %z{0}-- - 1 == 10000);
ok (1 * %z{0} == 10000);
ok (0 + %z{0}-- - 0 == 10000);
ok (1 + %z{0} == 10000);
ok (1 + %z{0}++ == 10000);
ok (0 + %z{0} == 10000);
ok (0 + --%z{0} + 1 == 10000);
ok (0 + ++%z{0} + 0 == 10000);
ok (%z{0} == 10000);

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

