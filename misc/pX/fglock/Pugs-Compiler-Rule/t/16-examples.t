use strict;
use warnings;
use Test::More tests => 2 * 6;

mkdir 'tmp' if !-e 'tmp';

sub test {
    my $name = shift;
    my $code = shift;
    my $expected = shift;
    my $module = ucfirst($name);
    my $pmfile = "tmp/$module.pm";
    unlink $pmfile if -f $pmfile;
    is system("$^X -Ilib util/compile_p6grammar.pl examples/$name.grammar > $pmfile"), 0, "$name.grammar compiles okay";
    my $cmd = qq{$^X -Ilib -Itmp -M$module -e "$code"};
    my $out = `$cmd`;
    chomp($out);
    is $out, $expected, "output of [ $cmd ] okay";
}

test('adder', "print Adder->add('3 + 23')->()", 26);
test('adder', "print Adder->add('532+49')->()", 581);

test('digits', "print Digits->count('49a3')->()", 3);

test('langs', "print My::VB->def('Dim a, b As double')->{'My::C.var_list'}", 'a, b');

test('langs2', "print My::VB->def('Dim a, b As double')->{'My::C.var_list'}", 'a, b ');

test('Grammar', "print Pugs::Grammar::Rule->rule('a b')->to", 3);

