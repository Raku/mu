use v6;
use Test;

plan 10;

=begin kwid

This file /exhaustivily/ tests the Test module. 

I try every variant of each Test function here
because we are using this module to test Pugs itself, 
so I want to be sure that the error is not coming from 
within this module.

We need to test that these functions produce 'not ok' at the
right times, too.  Here, we do that by abusing :todo to mean
"supposed to fail."  Thus, no ":todo" failure indicates
a missing feature.

If there is a bug in the implementation, you will see
a (non-TODO) failure or an unexpected success.

=end kwid

## ok

ok(2 + 2 == 4, '2 and 2 make 4');
ok(2 + 2 == 4, desc => '2 and 2 make 4');
ok(2 + 2 == 4, :desc('2 and 2 make 4'));

ok(2 + 2 == 5, desc => '2 and 2 doesnt make 5', todo => <bug>);
ok(2 + 2 == 5, :desc('2 and 2 doesnt make 5'), :todo(1));

## is

is(2 + 2, 4, '2 and 2 make 4');
is(2 + 2, 4, desc => '2 and 2 make 4');
is(2 + 2, 4, :desc('2 and 2 make 4'));

is(2 + 2, 5, todo => 1, desc => '2 and 2 doesnt make 5');
is(2 + 2, 5, :todo<feature>, :desc('2 and 2 doesnt make 5'));

