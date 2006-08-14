use v6-alpha;

use Test;

plan 22;

=kwid

<S02/Literals/"Characters indexed by" hex numbers
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

=kwid

<S02/Literals/"Characters indexed by" octal numbers"
              with "\o">

=cut

is("\o40", ' ', '\o40 normal space');
is("\o240", ' ', '\o240 non-breaking space');

is("\o[40]", ' ', '\o[40] normal space');
is("\o[240]", chr(160), '\o[240] non-breaking space');
is("\o[23072]", '☺', '\o[23072] wide hex character (SMILEY)');
is("\o[61021]", '我', '\o[61021] wide hex character (Chinese char)');
is("\o[6211", '\o[6211', 'broken "\o[6211"');
is("\o [6211]", '\o [6211]', 'broken "\o [6211]"');

is("\o[101,102,103]", 'ABC', '\o[list]');
is("\o[47540,54575]", '你好', '\o[a,b]');
is("\o101,102,103", 'A,102,103', '\olist not valid');
