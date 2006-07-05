use v6-pugs;

use Test;

plan 7;

=kwid

<S02/"Characters indexed by hex, octal, and decimal">

=cut

is("\x20", ' ', '\x20 normal space');
is("\xa0", ' ', '\xa0 non-breaking space');
is("\x[20]", ' ', '\x[20] normal space');
is("\x[a0]", chr(0xa0), '\x[a0] non-breaking space');
is("\x[263a]", '☺', '\x{263a} wide hex character (SMILEY)');
is("\x[41,42,43]", 'ABC', '\x[list]');
is("\x41,42,43", 'A,42,43', '\xlist not valid');
