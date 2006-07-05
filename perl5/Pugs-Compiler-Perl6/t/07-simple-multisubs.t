
use v6-**;

my $has_run = 0;
try {

say "1..2";

multi foo ($only_one_arg) {
    if $only_one_arg eq "only_one_arg"     { say "ok 1" } else { say "not ok 1" }
}

multi foo ($arg1, $arg2) {
    if $arg1 eq "arg1" and $arg2 eq "arg2" { say "ok 2" } else { say "not ok 2" }
}

foo "only_one_arg";
foo "arg1", "arg2";

$has_run = 1;

}

unless $has_run {
    say 'ok 1 # skip TODO emit multis';
    say 'ok 2 # skip TODO emit multis';
}
