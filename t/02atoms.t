use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..1";

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;

sub ok {
    my ($var, $loop) = @_;
    if ($var) {
	say("ok ", $loop, " # TODO var = ", $var);
    }
    else {
	say("not ok ", $loop, " # TODO");
    }
    $loop++;
}

eval '$bar = $::($foo)';
if ($bar) {
    say("ok ", $loop, " # TODO var = ", $bar);
} else {
    say("not ok ", $loop, " # TODO");
}

