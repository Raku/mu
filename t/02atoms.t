use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;

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

my @blah = '$foo', 'MY::$foo';
for (@blah) {
    my $bar;
    eval '$bar = $::($_)';
    ok $bar, $loop;
    $loop++;
}

say "1..", ($loop-1);
