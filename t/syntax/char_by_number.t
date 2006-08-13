use v6-alpha;

use Test;

plan 11;

=kwid

<S02/Literals/"Characters indexed by hex, octal, and decimal numbers"
              "\x123" "\x[123]" "\x[41,42,43]">

=cut

is("\x20", ' ', '\x20 normal space');
is("\xa0", ' ', '\xa0 non-breaking space');

is("\x[20]", ' ', '\x[20] normal space');
is("\x[a0]", chr(0xa0), '\x[a0] non-breaking space');
is("\x[263a]", '☺', '\x[263a] wide hex character (SMILEY)');
is("\x[6211]", '我', '\x[597d] wide hex character (Chinese char)');
is("\x[6211", '\x[6211', 'broken "\x[6211"');
is("\x [6211]", '\x [6211]', 'broken "\x [6211]"');

is("\x[41,42,43]", 'ABC', '\x[list]');
is("\x[4f60,597d]", '你好', '\x[a,b]');
is("\x41,42,43", 'A,42,43', '\xlist not valid');
