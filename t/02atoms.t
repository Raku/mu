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
    if ($_) {
	say("ok ", $loop, " # TODO var = ", $_);
    }
    else {
	say("not ok ", $loop, " # TODO");
    }
    $loop++;
}

eval '$bar = $::($foo)';
ok($bar, $loop);

