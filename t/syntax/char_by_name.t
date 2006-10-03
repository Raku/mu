use v6-alpha;

use Test;

plan 7;

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
