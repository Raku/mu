use v6-alpha;
use Test;
=qwid

  pugs is not recognizing unicode aliases like \c[LINE FEED] and \c[LF].

=cut

plan 3;

ok("\c[LINE FEED (LF)]",'\c[LINE FEED (LF)] works');
eval_ok(q{"\c[LINE FEED]"},'\c[LINE FEED] works');
eval_ok(q{"\c[LF]"},'\c[LF] works');
