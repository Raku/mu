role Parse::Rule::Strategy;

use Parse::Rule::Core;

=pod

C<assertion(&assert)> runs C<&assert>. If the result is true, is continues. If
it is false, it backtracks.

=cut

method assertion ($assert) {...}

=pod

C<empty> matches nothing, successfully.

=cut

method empty () {...}

=pod

C<concat($a, $b)> first tries to match C<$a>, then tries C<$b> where
C<$a> left off.  

=cut

method concat ($a, $b) {...}

=pod

C<alternate($a, $b)> tries C<$a>, and tries C<$b> if C<$a> fails.

=cut

method alternate ($a, $b) {...}

=pod

C<quantify($p, $low, $high)> tries to match C<$p> at least C<$low>
times, and at most C<$high> times.  If C<$low> is not given, it
defaults to 0.  If C<$high> is not given, it defaults to infinity.  So
C</a*/> can be written C<quantify(literal("a"))>.

C<quantify> also takes a C<:minimal> argument, which, if present, tells
the combinator to succeed as soon as it has matched C<$low> times, and
backtrack by matching more.

=cut

method quantify ($p, $low? = 0, $high? = Inf, :$minimal = 0) {...}

=pod

C<optional($p)> is pretty much the same as C<quantify($p,0,1)>,  but it
has its own combinator because it stores captures differently.  Instead
of creating another level of arrays in the captures, it simply doesn't
put anything there if it didn't match.

C<optional> also takes a C<:minimal> argument, which does the same thing
as it does above in C<quantify>.

=cut

method optional ($p, :$minimal = 0) {...}

=pod

C<capture($p, :num($num), :name($name))> will capture whatever C<$p> matched
into C<$match.capture_num[$num]> and C<$match.capture_name{$name}>.  C<$p> will
be given a fresh, empty C<$match.match> to fill (this introduces a new scope).
If no $num or $name is given, a new scope is introduced but the resulting object
is discarded (used for eg. <?ws>).

=cut

method capture ($p, Int :$num, Str :$name) {...}

=pod

C<mark($p, $name)> simply executes C<$p>, while setting up a
backtracking mark named C<$name>.  The name is scoped to C<$p>.  That
is, once the match succeeds beyond C<$p>, the mark is no longer valid.
This gives the impression of lexically scoped marks (but it may also
limit the engine semantically, we shall see).  See C<commit>.

=cut

method mark ($p, Str $name) {...}

=pod

C<commit($name)> will always succeed.  When backtracked over, it skips to the
C<mark> (see above) named C<$name>.  It's not clear what these mean for
non-backtracking engines.

=cut

method commit (Str $name) {...}

=pod

C<subrule($code)> executes $code, passing the match object as a parameter, and
it should return a I<compiled> rule (i.e.  a C<Rule> object, not a C<Parser>
object).  This rule is then incorporated into the match.  XXX it is not clear
that this is the optimal interface for this combinator.

=cut

method subrule (Code $code) {...}

# vim: ft=perl6 :
