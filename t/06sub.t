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
    say "ok 1 # subroutine at beginning";
} else {
    say "not ok 1 # subroutine at beginning";
}
$bar = "";
eval '$bar = check $foo';
if ($bar) { say "ok 2"; } else { say "not ok 2 # TODO subroutine at end"; }

sub check {
    return $_;
}
