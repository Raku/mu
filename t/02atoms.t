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
	say "ok  # TODO var = "~$_;
    }
    else {
	say "not ok # TODO";
    }
}

eval '$bar = $::($foo)';
ok($bar,$loop);

