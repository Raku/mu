use v6-alpha;

# Checking that testing is sane: subroutines

module Main {

say '1..4';

sub ok($num) {
    say "ok $num";
};

ok(1);
ok 2;

my $counter = 2;
sub ok_auto {
    $counter = $counter + 1;
    say "ok $counter";
};

ok_auto();
ok_auto;

}