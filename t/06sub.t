use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..1";

sub foobar ($var) {
    say "var=",$var;
    return $var;
}

sub check {
    return $_;
}

my $foo = "foo";
my $bar;
eval '$bar = foobar($foo); ';
say "bar=", $bar;
if ($foo eq $bar) {
    say "1 ok";
} else {
    say "1 not ok # TODO";
}

$bar = check $foo;
if ($bar) { say "2 ok"; } else { say "2 not ok"; }

