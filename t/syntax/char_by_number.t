use v6-alpha;

use Test;

plan 35;

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

=kwid

<S02/Literals/"Characters indexed by" decimal numbers"
              with "\d">

=cut

is("\d32", ' ', '\d32 normal space');
is("\d160", ' ', '\d160 non-breaking space');

is("\d[32]", ' ', '\d[32] normal space');
is("\d[160]", chr(160), '\d[240] non-breaking space');
is("\d[9786]", '☺', '\d[9786] wide hex character (SMILEY)');
is("\d[25105]", '我', '\d[25105] wide hex character (Chinese char)');
is("\d[6211", '\d[6211', 'broken "\d[6211"');
is("\d [6211]", '\d [6211]', 'broken "\d [6211]"');

is("\d[65,66,67]", 'ABC', '\d[list]');
is("\d[20320,22909]", '你好', '\d[a,b]');
is("\d65,66,67", 'A,66,67', '\dlist not valid');

# IRC note
# ---------------------------------------------------------------
# <agentzh> auderyt: is "\187 \132" valid in Perl 6? The synopses
#          don't mention this form of characters indexed by number.
# <audreyt> oh hm, wait, \187 indeed is not
# <audreyt> I don't know... I can see that \65 is not as clear
#           as \d65
# <agentzh> aye
# <audreyt> so I'm fine for retiring the \187 form

eval '"\123"' //
ok($!, '"\123" form is no longer valid Perl 6');
eval '"\040"' //
ok($!, '"\040" form is no longer valid Perl 6');
