use v6-alpha;

# Test closure traits in eval strings

use Test;

plan 18;

# L<S02/Closure traits/Code "generated at run-time" "still fire off"
#   "can't" "travel back in time" >

my ($code, $hist, $handle);

$code = '$handle = { START { $hist ~= "F" } }';
ok eval($code), 'eval START {...} works';
is $hist, undef, 'START {...} has not run yet';
is $handle(), 'F', 'START {...} fired';
is $handle(), 'F', 'START {...} fired only once';

$code = '$handle = { INIT { $hist ~= "I" } }';
ok eval($code), 'eval INIT {...} works';
is $hist, 'FI', 'INIT {...} already fired at run-time';
is $handle(), 'FI', 'INIT {...} fired only once';

$code = '$handle = { CHECK { $hist ~= "C" } }';
ok eval($code), 'eval CHECK {...} works';
is $hist, 'FIC', 'CHECK {...} fires at run-time';
is $handle(), 'FIC', 'CHECK {...} fired only once';

$code = '$handle = { BEGIN { $hist ~= "B" } }';
ok eval($code), 'eval BEGIN {...} works';
is $hist, 'FICB', 'CHECK {...} fired at run-time';
is $handle(), 'FICB', 'CHECK {...} fired only once';

END {
    is $hist, 'FICBE', 'the END {...} in eval has run already';
}

$code = '$handle = { END { $hist ~= "E" } }';
ok eval($code), 'eval END {...} works';
ok $handle, '$handle to the closure returned as expected';
is $hist, 'FICB', 'END {...} doesn\'t run yet';
is $handle(), undef, "END \{...\} doesn't run yet";
