#=let1.pl - Anwendung von let
my $prim = 17;

sub is_prim { 
	... # teste ob $prim prim ist
}

sub search_prim_within( $count ) {
	my @numbers := $prim...$prim+$count :by(2);
	let $prim;
	
	for @numbers -> $prim {
		return 1 if is_prim;
	}
	return 0;
}
