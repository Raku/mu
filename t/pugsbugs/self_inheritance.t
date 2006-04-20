#/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

do { role A does A; class B does A; B.new; }
do { class A is A; A.new }
These will  cause infinite loop. hmm, But I really don't know which part to
test in above example, A compile time error? Or a runtime error?

=cut

dies_ok { role RA does RA { }},
	"Testing `role A does A`";

dies_ok { class CA is CA { }},
	"Testing `class A is A`";





#B.new;

