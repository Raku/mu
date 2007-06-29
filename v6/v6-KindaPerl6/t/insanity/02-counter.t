use v6-alpha;

# Checking that testing is sane: counted tests

module Main {

say '1..4';

my $counter = 1;
#say "ok $counter";
say "ok " ~ $counter;

#$counter++;
++$counter;
#say "ok $counter";
say "ok ",$counter;

++$counter;
say 'ok ', $counter;

++$counter;
say 'ok ' ~ $counter;

}
