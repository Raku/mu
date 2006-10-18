use v6-alpha;

use Test;
plan 1;

my $value_from_BUILD;
#L<S12/"Construction and Initialization"/named arguments to bless passed to
#	CREATE BUILD>

class Foo { submethod BUILD ($value) { $value_from_BUILD = $value; } };

dies_ok { Foo.new("passed") }, 'positional args passed to new() should not be passed on to BUILD';
