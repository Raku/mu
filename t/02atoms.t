use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..3";

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;

sub ok ($var, $loop) {
    if ($var) {
	say("ok ", $loop, " # TODO var = ", $var);
    }
    else {
	say("not ok ", $loop, " # TODO");
    }
}


eval '$bar = $::($foo)';
ok ($bar, 1);
$bar = '';
eval '$bar = $::("MY::$foo")';
ok ($bar, 2);
$bar = '';
eval '$bar = $::($foobar)';
ok ($bar, 3);
