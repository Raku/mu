my @color = qw(red green blue);

my $x = any @color;

my $y = 'blue';

my $result = ($x eq $y) ?? "acceptable" :: 'unacceptable' ;

print "$result color\n";

  
