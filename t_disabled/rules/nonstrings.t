#!/usr/bin/pugs

use v6;
use Test;

plan 5;

=pod

L<S05/"Matching against non-strings">

=cut

# String-like things...

my $fh = open($?FILE);
rule monster { dr\wgon }; # contrived pattern which does not match itself; we're going to look for it in this file
rule cheese { camembert | cheddar  };
my $stream;
eval '$stream is from($fh)';

eval_ok('$stream ~~ /<cheese>/', 'rules on streams, positive', :todo<feature>); # should match
eval_ok('! ($stream ~~ /<monster>/)', 'rules on streams, negative', :todo<feature>); # shouldn't match

# And arrays...

my Dog $a;
my Cat $b;
my Fish $c;

my @array = ($a, $b, $c);

eval_ok('rule canine { <.isa(Dog)> }; @array ~~ /<canine>/', 'rules on an array - positive', :todo<feature>);
eval_ok('rule herbivore { <.isa(Antelope)> }; ! @array ~~ /<herbivore>/', 'rules on an array - negative', :todo<feature>);
# These seem to be failing for some sort of scoping error rather than a problem with the 
# rule matching itself.

# And matching against each element of an array... a different topic really, but it's still in
# that bit of the synopsis.

my @names = ('zaphod', 'ford', 'arthur', 'slartibartfast');
my $arrr = rule { ar };
eval_is('@names>>.match($arrr)', 2, 'matching with hyper-operator', :todo<feature>);
