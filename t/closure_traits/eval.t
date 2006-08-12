use v6-alpha;

# Test closure traits in eval strings

use Test;

plan 14;

# L<S02/Closure traits/Code "generated at run-time" "still fire off">

my ($code, $hist, $handle);

$code = '$handle = { INIT { $hist ~= "I" } }';
ok eval($code), 'eval INIT {...} works';
is $hist, 'I', 'INIT {...} already fired at run-time';
is $handle(), 'I', 'INIT {...} fired only once';

$code = '$handle = { CHECK { $hist ~= "C" } }';
ok eval($code), 'eval CHECK {...} works';
is $hist, 'IC', 'CHECK {...} fires at run-time';
is $handle(), 'IC', 'CHECK {...} fired only once';

$code = '$handle = { BEGIN { $hist ~= "B" } }';
ok eval($code), 'eval BEGIN {...} works';
is $hist, 'ICB', 'CHECK {...} fired at run-time';
is $handle(), 'ICB', 'CHECK {...} fired only once';

END {
    is $hist, 'ICBE', 'the END {...} in eval has run already';
}

$code = '$handle = { END { $hist ~= "E" } }';
ok eval($code), 'eval END {...} works';
ok $handle, '$handle to the closure returned as expected';
is $hist, 'ICB', 'END {...} doesn\'t run yet';
is $handle(), undef, "END \{...\} doesn't run yet";
