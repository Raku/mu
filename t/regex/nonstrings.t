use v6;

use Test;

plan 5;

=pod

L<S05/"Matching against non-strings">

=cut

# String-like things...

my $fh = open($?FILE);
regex monster { dr\wgon }; # contrived pattern which does not match itself; we're going to look for it in this file
regex cheese { camembert | cheddar  };
my $stream;
eval '$stream := cat =$fh';

ok(eval('$stream ~~ /<cheese>/'), 'rules on streams, positive', :todo<feature>); # should match
ok(eval('! ($stream ~~ /<monster>/)'), 'rules on streams, negative'); # shouldn't match

# And arrays...

my Dog $a;
my Cat $b;
my Fish $c;

my @array = ($a, $b, $c);

ok(eval('regex canine { <.isa(Dog)> }; @array ~~ /<canine>/'), 'rules on an array - positive', :todo<feature>);
ok(eval('regex herbivore { <.isa(Antelope)> }; ! @array ~~ /<herbivore>/'), 'rules on an array - negative', :todo<feature>);
# These seem to be failing for some sort of scoping error rather than a problem with the 
# regex matching itself.

# And matching against each element of an array... a different topic really, but it's still in
# that bit of the synopsis.

my @names = ('zaphod', 'ford', 'arthur', 'slartibartfast');
my $arrr = regex { ar };
is(eval('@names>>.match($arrr)'), 2, 'matching with hyper-operator', :todo<feature>);
