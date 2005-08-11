#=interp3.pl - Interpolation in Perl 6
sub hallo {
	"Hallo, @_[0]!" # $_ würde aber auch gehen!
}

say "Otto sagt: &hallo('Hund')"; # Funktionsaufruf
