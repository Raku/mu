use v6;

=pod

This is a test file.  Whee!

=cut

say "1..1";

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;

eval '$bar = $::($foo)';

if ($bar) {
    say "ok 1 # TODO bar = " ~ $bar;
}
else {
    say "not ok 1 # TODO"
}
