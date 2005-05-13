
use v6;
module Motd;

sub finger (*@arr) returns Str is export {
		my @default = (
			"boring",
			"missing examples",
		);
	  pick any @arr || @default
}

sub report (@x) returns Str is export {
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
sub clear returns Void is export{
	system(($?OS eq any<MSWin32 mingw cygwin>) ?? 'cls' :: 'clear');
}

sub max (Array @x) returns Int{
	my $max;
	for @x->$try{
	$max =	$max > $try ?? $max :: $try; 
	}
	$max.int
}	

sub whisper_about (Int $sizeof_crowd, Array ?@x) returns Hash is
export{
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

sub append_last ($x,$string is rw,$pass){
	if $pass > 2 {
		chop $string;
		"$string and " ~  
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
		chop $string;
		"$string and " ~
		pick any (
			"$x",
			"is $x",
		); 
	}else{
	  "$string $x";
	}
}

sub matchval ($x,%x,Int ?$cap) returns Array is export{
	my @matches;
	for (%x.pair)->$pair{
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



