use v6-alpha;
use Test;
plan 2;

# L<S29/"Hash"/"=item exists">

sub gen_hash {
	my %h{'a'..'z'} = (1..26);
	return %h;
};

{
	my %h1 = gen_hash;
	my %h2 = gen_hash;

	my $b = %h1<b>;
	is (exists %h1, 'a'), 1, "Test existance for singe key. (Indirect notation)";
	is (%h1.exists('a')), 1, "Test existance for singe key. (method call)";
};
