use v6;

sub finger (*@arr) returns Str {
		my @default = (
			"boring",
			"missing examples",
		);
	  pick any @arr || @default
}

{
 my $iter = 0;
 my $str  = '';
 multi sub report () returns Str {my $a = $str; $str = ''; $iter=0; $a;} 
 multi sub report (*$x, *@x) returns Void {
 	$iter++;
	if(@x){
		$str ~= addtolist($x);
	}else{
		$str = append_last($x,$str,$iter);
	}
	report @x;
 }
}

#Autrijus's "cls"
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
        %terms{finger(@x)}++;
    }
    %terms
}

sub addtolist ($x){
	" $x,";	
}

sub append_last ($x,$string,$pass){
  my $rwstring = $string; 
	if $pass > 1 {
		chop $rwstring;
		"$rwstring and $x"; 
	}else{
	  "$rwstring $x";
	}
}

sub matchval ($x,%x,Int ?$cap) returns Array {
	my @matches;
	for (%x.pairs)->$pair{
		if $pair[1] eq $x { push @matches,$pair[0] }  
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


