use v6-alpha;

# Test closure traits in eval strings

use Test;

plan 12;

# L<S02/Closure traits/Code "generated at run-time" "still fire off">

my ($code, $hist, $handle);

$code = '$handle = { INIT { $hist ~= "I" } }';
ok eval($code), 'eval INIT {...} works';
is $handle(), 'I', 'INIT {...} fires at run-time';
is $handle(), 'I', 'INIT {...} fires only once';

$code = '$handle = { CHECK { $hist ~= "C" } }';
ok eval($code), 'eval CHECK {...} works';
is $handle(), 'IC', 'CHECK {...} fires at run-time';
is $handle(), 'IC', 'CHECK {...} fires only once';

$code = '$handle = { BEGIN { $hist ~= "B" } }';
ok eval($code), 'eval BEGIN {...} works';
is $handle(), 'ICB', 'CHECK {...} fires at run-time';
is $handle(), 'ICB', 'CHECK {...} fires only once';

END {
    is $hist, 'ICBE', 'the END {...} in eval has run already';
}

$code = '$handle = { END { $hist ~= "E" } }';
ok eval($code), 'eval END {...} works';
is $handle(), undef, "END \{...\} doesn't run yet";
