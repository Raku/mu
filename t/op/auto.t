#!/usr/bin/pugs

use v6;
require Test;

# again, from Perl 5

plan 37;

my $x = 10000;
ok (0 + ++$x - 1 == 10000);
ok (0 + $x-- - 1 == 10000);
ok (1 * $x == 10000);
ok (0 + $x-- - 0 == 10000);
ok (1 + $x == 10000);
ok (1 + $x++ == 10000);
ok (0 + $x == 10000);
ok (0 + --$x + 1 == 10000);
ok (0 + ++$x + 0 == 10000);
ok ($x == 10000);

my @x;
@x[0] = 10000;
ok (0 + ++@x[0] - 1 == 10000);
ok (0 + @x[0]-- - 1 == 10000);
ok (1 * @x[0] == 10000);
ok (0 + @x[0]-- - 0 == 10000);
ok (1 + @x[0] == 10000);
ok (1 + @x[0]++ == 10000);
ok (0 + @x[0] == 10000);
ok (0 + --@x[0] + 1 == 10000);
ok (0 + ++@x[0] + 0 == 10000);
ok (@x[0] == 10000);

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
ok (++$foo eq '100');
$foo = 'a0';
ok (++$foo eq 'a1');
$foo = 'Az';
ok (++$foo eq 'Ba');
$foo = 'zz';
ok (++$foo eq 'aaa');
$foo = 'A99';
ok (++$foo eq 'B00');
# EBCDIC guards: i and j, r and s, are not contiguous.
$foo = 'zi';
ok (++$foo eq 'zj');
$foo = 'zr';
ok (++$foo eq 'zs');

