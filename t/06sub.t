use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..1";

sub foobar {
    my $var = shift;
    print "var=",$var;
    return $var;
}

my $foo = "foo";
my $bar;
eval '$bar = foobar($foo); ';
print "bar=", $bar;
if ($foo eq $bar) {
    print "1 ok";
} else {
    print "1 not ok # TODO";
}

$bar = check $foo;
if ($bar) { print "2 ok"; } else { print "2 not ok"; }

sub check {
    return $_;
}
