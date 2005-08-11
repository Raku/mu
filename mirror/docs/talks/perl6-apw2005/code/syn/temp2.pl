#=temp2.pl - Anwendung von temp
sub beispiel {
	my $i = 1;
	
	say "a: i=$i\n";
	while $i < 100 {
		temp $i;
		
		$i = 1000;
		say "b: i=$i";
	}
}
