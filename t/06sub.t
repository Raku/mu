use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..2";

sub foobar ($var) {
    return $var;
}

my $foo = "foo";
my $bar;
eval '$bar = foobar($foo); ';
if ($foo eq $bar) {
    say "1 ok";
} else {
    say "1 not ok # TODO";
}
$bar = "";
eval '$bar = check $foo';
if ($bar) { say "2 ok"; } else { say "2 not ok #TODO"; }

sub check {
    return $_;
}
