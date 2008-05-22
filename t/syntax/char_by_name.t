use v6;

use Test;

plan 9;

# XXX [TODO] more tests in other Unicode charset.

# L<S02/Literals/interpolate Unicode "by name" using "\c"
#   and "square brackets">

is "\c[LEFT CORNER BRACKET]", "「", '\c[LEFT CORNER BRACKET]';
is "\c[RIGHT WHITE CORNER BRACKET]", "』", '\c[RIGHT WHITE CORNER BRACKET]';
is "\c[FULLWIDTH RIGHT PARENTHESIS]", "）", '\c[FULLWIDTH RIGHT PARENTHESIS]';
is "\c[LEFT DOUBLE ANGLE BRACKET]", "《", '\c[LEFT DOUBLE ANGLE BRACKET]';

is("\c[LINE FEED (LF)]", "\d10", '\c[LINE FEED (LF)] works');
is("\c[LINE FEED]", "\d10", '\c[LINE FEED] works');
is("\c[LF]", "\d10", '\c[LF] works');

# L<S02/Literals/"Multiple codepoints constituting a single character">
is eval('"\c[LATIN CAPITAL LETTER A, LATIN CAPITAL LETTER B]"'), 'AB', 'two letters in \c[]';
is eval('"\c[LATIN CAPITAL LETTER A, COMBINING GRAVE ACCENT]"'), "\x[0041,0300]", 'letter and combining char in \c[]';
