use v6;

=pod

This is a test file.  Whee!

=cut
my $loop = 1;
say "1..10";

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

my $_;

sub callerunderscore {
    return "wrong one, needed to avoid errors"
}

eval '
    sub callerunderscore (?$foo = $CALLER:_) { 
        return "-" ~ $foo ~ "-"
    }
';

if (callerunderscore("foo") eq "-foo-") { say "ok 5" } else { say "not ok 5 # TODO CALLER::" }
if (callerunderscore(1) eq "-1-") { say "ok 6" } else { say "not ok 6 # TODO CALLER::" }
$_ = "foo";
if (callerunderscore() eq "-foo-") { say "ok 7" } else { say "not ok 7 # TODO CALLER::" }
$_ = "bar";
if (callerunderscore() eq "-bar-") { say "ok 8" } else { say "not ok 8 # TODO CALLER::" }
for ("quux") { 
    if (callerunderscore() eq "-quux-") { say "ok 9" } else { say "not ok 9 # TODO CALLER::" }
}
if (callerunderscore() eq "-bar-") { say "ok 10" } else { say "not ok 10 # TODO CALLER::" }



