use v6;

=pod

This is a test file.  Whee!

=cut

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;

eval ' $bar = $::($foo); ';

if ($bar) {
    print "ok bar = "~$bar;
}
    

