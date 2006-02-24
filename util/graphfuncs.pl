#!env perl -w
use strict;
use GraphViz;

my $signature = qr/^([\w']*) ::/o;
my $fn;
my %funcs;
my $g = GraphViz->new();

while (<>) {
	if($_ =~ $signature) { $fn = $1; $g->add_node($fn); next }
	if($fn) { $funcs{$fn} .= $_ }
}

for my $key (keys %funcs) {
	for my $func (keys %funcs) {
		next if $func eq $key;
		#print "$key -> $func\n"
		$g->add_edge($key, $func) if($funcs{$key} =~ $func)
	}
}

print $g->as_png