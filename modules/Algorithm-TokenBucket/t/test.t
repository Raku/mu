#!/usr/bin/perl6

use Test;
BEGIN { plan 10 }
use Algorithm::TokenBucket;

my Algorithm::TokenBucket $bucket .= new(rate => 25/1, burst_size => 4);
ok $bucket ~~ Algorithm::TokenBucket;
is $bucket.rate,      25, 'info_rate init';
is $bucket.burst_size, 4, 'burst_size init';
# -- Note! This test may break if the OS suspends for > 0.3s or < 0.3s!
sleep 0.3;
ok $bucket.conform(0),  '0 conforms';
ok $bucket.conform(4),  '4 conforms';
ok !$bucket.conform(5), '5 does not conform';
$bucket.count(1);
ok !$bucket.conform(4), '4 no more conforms';
ok $bucket.conform(3),  'only 3 does';
$bucket.count(1);
$bucket.count(1);
$bucket.count(1);
ok !$bucket.conform(1), 'even 1 conforms no more';

# pass 50 within 2 seconds
my $traffic = 50;
my $time    = time;
while time - $time < 2 {
  if $bucket.conform(1) {
    $bucket.count(1);
    $traffic--;
  }
}
is $traffic, 0, '50 in 2 seconds';
