use strict;
use warnings;
use Test::More tests => 10;

mkdir 'tmp' if !-e 'tmp';

sub test {
    my $name = shift;
    my $code = shift;
    my $exit_code = shift;
    my $expected = shift;
    my $expected_count = shift;
    my $module = ucfirst($name);
    my $pmfile = "tmp/$module.pm";
    unlink $pmfile if -f $pmfile;
    my $exit = system("$^X -Ilib util/compile_p6grammar.pl -T -D examples/$name.grammar > $pmfile");
    #warn "EXIT CODE: $exit";
    $exit = $exit >> 8;
    #warn "EXIT CODE (2): $exit";
    is $exit, $exit_code, "$name.grammar compiles okay";
    if ($exit == 0) {
        my $cmd = "$^X -Ilib -Itmp -M$module -e '$code'";
        my $out = `$cmd`;
        chomp($out);
        my $count = $out =~ s/^>>(?:BEGIN|END) \w+<<[^\n]+\n//gsm;
        is $out, $expected, "output of [ $cmd ] okay";
        is $count, $expected_count, 'debugging output count ok';
    }
}

test('adder', 'print Adder->add("3 + 23")->(), "\n"', 255);
#die;
test('adder', 'print Adder->add("532+49")->(), "\n"', 255);

test('digits', 'print Digits->count("49a3")->(), "\n"', 255);

test('langs', 'print My::VB->def("Dim a, b As double")->{"My::C.var_list"}, "\n"', 0, 'a, b', 96);

test('langs2', 'print My::VB->def("Dim a, b As double")->{"My::C.var_list"}, "\n"', 0, 'a, b ', 100);

test('Grammar', 'print Pugs::Grammar::Rule->rule("a b")->to', 255);

