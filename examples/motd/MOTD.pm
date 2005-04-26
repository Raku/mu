use v6;

sub finger (*@arr) returns Str {
		my @default = (
			"boring",
			"missing examples",
		);
	  pick any @arr || @default
}

sub report (@x) returns Str{
 	my $str = '';
	my $iter = 0 ;
	for @x -> $x{
	  if ++$iter == +@x {
	  	$str = append_last($x,$str,$iter);
		return $str;
      }else{
		$str ~= addtolist($x);
	  }
	}
}	


#Autrijus' "cls"
sub clear returns Void {
	system(($?OS eq any<MSWin32 mingw cygwin>) ?? 'cls' :: 'clear');
}

sub max (Array @x) returns Int{
	my $max;
	for @x->$try{
	$max =	$max > $try ?? $max :: $try; 
	}
	$max.int
}	

sub whisper_about (Int $sizeof_crowd, Array ?@x) returns Hash {
    my %terms;
    for 1 .. $sizeof_crowd {
		my $phrase = finger @x;
	   %terms{$phrase}++
    }
    %terms
}

sub addtolist ($x){
	" $x,";	
}

sub append_last ($x,$string,$pass){
    my $rwstring = $string; 
	if $pass > 2 {
		chop $rwstring;
		"$rwstring and " ~  
		pick any (
			"$x",
			"is $x",
			"$x, period",
			"$x, as well",
			"$x, besides",
			"$x, too",
			"$x, to boot"
			); 
	}elsif $pass > 1 {
		chop $rwstring;
		"$rwstring and " ~
		pick any (
			"$x",
			"is $x",
		); 
	}else{
	  "$rwstring $x";
	}
}

sub matchval ($x,%x,Int ?$cap) returns Array {
	my @matches;
	for (%x.pairs)->$pair{
		if $pair.value eq $x { push @matches,$pair.key }  
		if $cap && +@matches == $cap { return @matches }
	}
	@matches
} 

sub parse_args (Str $x) returns Array {
	my @args = split ',', $x;
	@args
}

1;
=end


