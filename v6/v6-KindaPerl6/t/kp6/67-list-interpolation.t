use v6-alpha;

# test list interpolation in parameter list

BEGIN {
	say "1..2";
}

sub foo ($a, $b){
    if $a != 1 { print "not " }
	say "ok $a";
    if $b != 2 { print "not " }
	say "ok $b";
}
my @list = (1, 2);
foo(|@list);
