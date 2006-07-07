use v6-alpha;

use Test;

plan 1;

my $junc = 0|1|2;
my @a = (0,1,2);
my $bool = Bool::False;
try { $bool = (@a[$junc] == $junc) };
ok $bool, :todo<bug>;
