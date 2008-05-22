use v6;
use Test;

# Tests for auto-increment and auto-decrement operators
# again, from Perl 5

plan 43;

#L<S03/Autoincrement precedence>

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
is(0 + ++@x[0] - 1, $base);

is(0 + --@x[0] + 0, $base);
is(@x[0],           $base);

my %z;
%z{0} = 10000;
ok(0 + ++%z{0} - 1 == 10000);
ok(0 + %z{0}-- - 1 == 10000);
ok(1 * %z{0} == 10000);
ok(0 + %z{0}-- - 0 == 10000);
ok(1 + %z{0} == 10000);
ok(1 + %z{0}++ == 10000);
ok(0 + %z{0} == 10000);
ok(0 + --%z{0} + 1 == 10000);
ok(0 + ++%z{0} + 0 == 10000);
ok(%z{0} == 10000);

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

# test magical autodecrement

$foo = '100';
is(--$foo, '99');

$foo = 'a1';
is(--$foo, 'a0');

$foo = 'Ba';
is(--$foo, 'Az');

$foo = 'aaa';
is(--$foo, 'zz');

$foo = 'B00';
is(--$foo, 'A99');

$foo = 'A00';
dies_ok( { --$foo }, 'autodecrementing A00 fails' );

