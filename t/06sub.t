use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..4";

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

sub twice { $_ * 2 }
if (twice(5) == 10) { say "ok 3" } else { say "not ok 3" }
if (eval 'twice(5 - 3) == 4') { say "ok 4" } else { say "not ok 4" }
