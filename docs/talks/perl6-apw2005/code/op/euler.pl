#=euler.pl - Berechnung der Euler'schen Zahl in Perl 6
use v6;
sub infix:«>?» ($a, $b) { $a > $b ?? $!!:: $b }
sub postfix:<!> ($x) { [*] 1..$x >? 1 }
sub e {
	my $ret = 0;

	for 0..1000 -> $i {
		my $old = $ret;
		$ret += 1 / $i!;
		return $ret if $ret-$old < 1e-50
	}	
}
say "e={e()}";
