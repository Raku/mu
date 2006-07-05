
# Checking that testing is sane: subroutines

use v6-**;

say '1..5';

sub ok($num) {
    say "ok $num";
}

ok(1);
ok 2;

my $counter = 2;
sub ok_auto {
    ++$counter;
    say "ok $counter";
}

ok_auto();
ok_auto;

sub run_sub(Code &code) {
    code();
}


run_sub( -> $blah {  say "ok 5"; }
       );

