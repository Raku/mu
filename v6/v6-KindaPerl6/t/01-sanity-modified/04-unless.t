use v6-alpha;

# Checking that testing is sane: if

module Main {
    
say '1..2';

my $x = '0';

unless ($x eq $x) { say 'not ok 1' } else { say 'ok 1' };
unless ($x ne $x) { say 'ok 2' } else { say 'not ok 2' };

}
