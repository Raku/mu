#!/usr/bin/pugs
use v6;

my @g = ('.') xx 9;
my $cnt = 0;
for (@g)->$x { 
	print "$x \t"; 
	print "\n" if ( !( ++$cnt % 3 ) ); 
}        
my $player;
my %player = ('X','Player 1','O','Player 2');

my %entered;
my $choice = any (1 .. 9);

my $tmp = 0;
while ($tmp < 9)
{
    $player = ( $tmp % 2 ) ?? 'O' :: 'X';
    say %player{$player} ," Enter the Position [1-9]:";
    my $in = =$IN;
    if (!$in ~~ rx:perl5/[1-9]/) {
	say " Please enter digits from 1-9 \n";
	eval 'next';
    }
    if ($in == $choice) {
    	$in --;
	if (%entered.exists($in)) {
		say  "Element already entered at $in";
		eval 'next';
	}
	%entered{$in} ++;
	$tmp ++;
    } else {
    	say "Please enter a value within 1-9";  
    	$tmp --;
    	eval 'next';  
    }
    
	@g[$in] = $player;
	for  (
        [ 0, 1, 2 ],
        [ 3, 4, 5 ],
        [ 6, 7, 8 ],
        [ 0, 3, 6 ],
        [ 1, 4, 7 ],
        [ 2, 5, 8 ],
        [ 0, 4, 8 ],
        [ 2, 4, 6 ]
      ) -> @c
    {
          if (join ('',@g[@c]) ~~ rx:perl5/([XO])\1\1/) {
             say "  %player{$player} Wins \n";
              exit;
           }
    }

	for (@g) { print $_; print "\n" if ( !( ++ $cnt % 3 ) ); }        
}
