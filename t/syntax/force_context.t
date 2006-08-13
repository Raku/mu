use v6-alpha;

use Test;

=kwid

Context forcing operators

=cut

plan 36;

# numeric (+) context
{
    my $a = '2 is my favorite number';
    is(ref(+$a), 'Num', 'it is forced into a Num');
    is(+$a, 2, 'forced into numeric context');

    my $b = 'Did you know that, 2 is my favorite number';
    is(ref(+$b), 'Num', 'it is forced into a Num');
    is(+$b, 0, 'non numbers forced into numeric context are 0');
}

# numeric (-) context
{
    my $a = '2 is my favorite number';
    is(ref(-$a), 'Num', 'it is forced into a Num');
    is(-$a, -2, 'forced into numeric context');

    my $b = 'Did you know that, 2 is my favorite number';
    is(ref(-$b), 'Num', 'it is forced into a Num');
    is(-$b, 0, 'non numbers forced into numeric context are 0');
}

# string context
{
    my $a = 10.500000;
    is(ref(~$a), 'Str', 'it is forced into a Str');
    is(~$a, '10.5', 'forced into string context');

    my $b = -100;
    is(ref(~$b), 'Str', 'it is forced into a Str');
    is(~$b, '-100', 'forced into string context');

    my $c = -100.1010;
    is(ref(~$c), 'Str', 'it is forced into a Str');
    is(~$c, '-100.101', 'forced into string context');
}

# boolean context
{
    my $a = '';
    is(ref(?$a), 'Bool', 'it is forced into a Bool');
    ok(!(?$a), 'it is forced into boolean context');

    my $b = 'This will be true';
    is(ref(?$b), 'Bool', 'it is forced into a Bool');
    ok(?$b, 'it is forced into boolean context');

    my $c = 0;
    is(ref(?$c), 'Bool', 'it is forced into a Bool');
    ok(!(?$c), 'it is forced into boolean context');

    my $d = 1;
    is(ref(?$d), 'Bool', 'it is forced into a Bool');
    ok(?$d, 'it is forced into boolean context');
}

# ! boolean context
{
    my $a = '';
    is(ref(!$a), 'Bool', 'it is forced into a Bool');
    ok(!$a, 'it is forced into boolean context');

    my $b = 'This will be true';
    is(ref(!$b), 'Bool', 'it is forced into a Bool');
    ok(!(!$b), 'it is forced into boolean context');

    my $c = 0;
    is(ref(!$c), 'Bool', 'it is forced into a Bool');
    ok(!$c, 'it is forced into boolean context');

    my $d = 1;
    is(ref(!$d), 'Bool', 'it is forced into a Bool');
    ok(!(!$d), 'it is forced into boolean context');
}

# int context
{
    my $a = '2 is my favorite number';
    is(ref(int($a)), 'Int', 'it is forced into a Int');
    is(+$a, 2, 'forced into integer context');

    my $b = 'Did you know that, 2 is my favorite number';
    is(ref(int($b)), 'Int', 'it is forced into a Int');
    is(int($b), 0, 'non numbers forced into integer context are 0');

    my $c = 1.21122111;
    is(ref(int($c)), 'Int', 'it is forced into a Int');
    is(int($c), 1, 'float numbers forced into integer context are 0');
}
