use v6;
require Test;

plan(5);

my $x = 3.14159265;
my $y = -3.14159265;

ok(int(-1) == -1);
ok(int(0) == 0);
ok(int(1) == 1);
ok(int($x) == 3);
ok(int($y) == -3);
