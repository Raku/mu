use v6-alpha;

use Test;

=kwid

Parse problem with //= and hash 

=cut

plan 3;

# hash {} gets parsed as two items when used with //=

my %hash;
%hash<foo> //= hash();
%hash<bar> //= hash;
my $h_ref;
$h_ref  //= hash();
is(%hash<foo>.WHAT, ::Hash, "Parses as two items");
is(%hash<bar>.WHAT, ::Hash, "Parens do not help");
is($h_ref.WHAT,     ::Hash, "It is not limited to hash values");
