# read from STDIN and print

my @color = qw(red green blue);

my $x = any @color;


say "enter a colour: ";
my $y = =$*IN;

my $result = ($x eq $y) ?? "acceptable" :: 'unacceptable' ;

print "$y is an $result color:\n";


