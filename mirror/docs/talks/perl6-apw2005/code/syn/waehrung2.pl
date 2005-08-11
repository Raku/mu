#=waehrung2.pl - Perl 6 Version
use v6;

sub £to€ { $_ * 1.4656 }

# interpolierter Funktions-Aufruf
say "100£ sind {£to€(100)}€";
# gibt '100£ sind 146.56€' aus
