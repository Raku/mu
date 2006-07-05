# #!/usr/bin/pugs

# Checking that testing is sane: Test.pm

use v6-pugs;
use Test;

my $has_run = 0;
try {

plan 1;

my $x = '0';
ok $x == $x;

$has_run = 1;

}

unless $has_run {
    say "1..1";
    say "not ok 1 # Can't load Test.pm";
}
