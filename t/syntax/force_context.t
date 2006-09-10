use v6-alpha;

use Test;

=kwid

Context forcing operators

=cut

plan 40;

# L<S02/Context/numeric "+">
# numeric (+) context
{
    my $a = '2 is my favorite number';
    isa_ok(+$a, Num, 'it is forced into a Num');
    is(+$a, 2, 'forced into numeric context');

    my $b = 'Did you know that, 2 is my favorite number';
    isa_ok(+$b, Num, 'it is forced into a Num');
    is(+$b, 0, 'non numbers forced into numeric context are 0');
}

# L<S03/Changes to Perl 5 operators/"-TERM" "coerced to numeric">
# numeric (-) context
{
    my $a = '2 is my favorite number';
    isa_ok(-$a, Num, 'it is forced into a Num');
    is(-$a, -2, 'forced into numeric context');

    my $b = 'Did you know that, 2 is my favorite number';
    isa_ok(-$b, Num, 'it is forced into a Num');

    # doubly-negated because -0 === 0 isn't neccessarily true
    is(-(-$b), 0, 'non numbers forced into numeric context are 0');
}

# L<S02/Context/string "~">
# L<S03/Changes to Perl 5 operators/Unary ~ string context>
# string context
{
    my $a = 10.500000;
    isa_ok(~$a, Str, 'it is forced into a Str');
    is(~$a, '10.5', 'forced into string context');

    my $b = -100;
    isa_ok(~$b, Str, 'it is forced into a Str');
    is(~$b, '-100', 'forced into string context');

    my $c = -100.1010;
    isa_ok(~$c, Str, 'it is forced into a Str');
    is(~$c, '-100.101', 'forced into string context');
}

# L<S02/Context/boolean "?">
# L<S03/Changes to Perl 5 operators/"?" imposes boolean context>
# boolean context
{
    my $a = '';
    isa_ok(?$a, Bool, 'it is forced into a Bool');
    ok(!(?$a), 'it is forced into boolean context');

    my $b = 'This will be true';
    isa_ok(?$b, Bool, 'it is forced into a Bool');
    ok(?$b, 'it is forced into boolean context');

    my $c = 0;
    isa_ok(?$c, Bool, 'it is forced into a Bool');
    ok(!(?$c), 'it is forced into boolean context');

    my $d = 1;
    isa_ok(?$d, Bool, 'it is forced into a Bool');
    ok(?$d, 'it is forced into boolean context');

    my $arrayref is context = list(1,2,3);
    my $boo is context = 37;
    eval_ok '?(@$+arrayref)', '?(@$arrayref) syntax works';
    eval_ok '?(@($+arrayref))', '?(@($arrayref)) syntax works';
}

# L<S03/Changes to Perl 5 operators/"!TERM" "coerced to boolean">
# ! boolean context
{
    my $a = '';
    isa_ok(!$a, Bool, 'it is forced into a Bool');
    ok(!$a, 'it is forced into boolean context');

    my $b = 'This will be true';
    isa_ok(!$b, Bool, 'it is forced into a Bool');
    ok(!(!$b), 'it is forced into boolean context');

    my $c = 0;
    isa_ok(!$c, Bool, 'it is forced into a Bool');
    ok(!$c, 'it is forced into boolean context');

    my $d = 1;
    isa_ok(!$d, Bool, 'it is forced into a Bool');
    ok(!(!$d), 'it is forced into boolean context');

    my $arrayref is context = list(1,2,3);

    eval_ok '!(!(@$+arrayref))', '!(@$arrayref) syntax works';
    eval_ok '!(!(@($+arrayref)))', '!(@($arrayref)) syntax works';
}

# int context
{
    my $a = '2 is my favorite number';
    isa_ok(int($a), Int, 'it is forced into a Int');
    is(+$a, 2, 'forced into integer context');

    my $b = 'Did you know that, 2 is my favorite number';
    isa_ok(int($b), Int, 'it is forced into a Int');
    is(int($b), 0, 'non numbers forced into integer context are 0');

    my $c = 1.21122111;
    isa_ok(int($c), Int, 'it is forced into a Int');
    is(int($c), 1, 'float numbers forced into integer context are 0');
}
