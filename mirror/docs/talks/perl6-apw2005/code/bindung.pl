#=Zuweisungsoperator
$x = $y; # wie bei Perl 5

#=Bindung (dynamisch)
my $x = 'Just Another';
# $x wird nun zur Laufzeit an $y gebunden
my $y := $x;
$y = 'Perl Hacker'; # $x ist nun auch 'Perl Hacker'
	
#=Bindung (statisch)
my $y ::= $x # Bindung zur Compile-Zeit
