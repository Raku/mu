use v6-alpha;

# test list interpolation in parameter list

BEGIN {
	say "1..2";
}

sub foo ($a, $b){
	say "ok $a";
	say "ok $b";
}
my @list = (1, 2);
foo(@list);
