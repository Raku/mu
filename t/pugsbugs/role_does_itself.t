#/usr/bin/pugs

use v6;
use Test;

plan 1;

=pod

do { role A does A; class B does A; B.new; }
Will cause infinite loop. hmm, But I really don't know which part to test
in above example, A compile time error? Or a runtime error?

=cut

dies_ok { role A does A { }},
	"Testing `role A does A`";


role A does A {};
class B does A {};

#B.new;

