use v6-alpha;

use Test;

plan 3;


# L<S06/"The &?ROUTINE object" /current routine name/>
# L<S02/"Names" /Which routine am I in/>
sub foo { return &?ROUTINE.name }
is(foo(), '&main::foo', 'got the right routine name');

my $bar = sub { return &?ROUTINE.name };
is($bar(), '<anon>', 'got the right routine name (anon-block)');

my $baz = try { &?ROUTINE.name };
ok(not(defined $baz), '&?ROUTINE.name not defined outside of a routine');
