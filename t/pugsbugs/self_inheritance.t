#/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

xinming audreyt: class A is A { };    <---  This error is reported at compile time or runtime?
xinming I mean, it will reported when it sees `class A is A` or, when A.new is invoked
audreyt I suspect compile time is the correct answer

=cut

dies_ok { role RA does RA { };},
	"Testing `role A does A`";

dies_ok { class CA is CA { };},
	"Testing `class A is A`";

