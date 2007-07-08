use strict;
use warnings;
use Test::More 'no_plan';

sub test {
    my $name = shift;
    my $code = shift;
    my $expected = shift;
    my $module = ucfirst($name);
    my $pmfile = $module . '.pm';
    unlink $pmfile if -f $pmfile;
    is system("$^X -Ilib util/compile_p6grammar.pl examples/$name.grammar > $pmfile"), 0, "$name.grammar compiles okay";
    my $cmd = "$^X -M$module -e '$code'";
    my $out = `$cmd`;
    chomp($out);
    is $out, $expected, "output of [ $cmd ] okay";
}

test('adder', 'print Adder->add("3 + 23")->(), "\n"', 26);
test('adder', 'print Adder->add("532+49")->(), "\n"', 581);

SKIP: {
    skip "digits.grammar loops", 2;
    test('digits', 'print Digits->count("49a3")->(), "\n"', 3);
};

