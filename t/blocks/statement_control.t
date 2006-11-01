use v6-alpha;

use Test;
plan 1;

#L<S04/Statement parsing/the statement_control category>
my $sub = -> $x { "got: $x" };
is( eval( q{ &statement_control:<for>(1, $sub) } ), "got: 1", 'statement_control:<for> with an anonymous sub', :todo );
