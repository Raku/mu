#=interp2.pl - Interpolation in Perl 5
sub hallo {
	"Hallo, $_[0]!"
}

print "Otto sagt: @{[hallo('Hund')]}\n";
