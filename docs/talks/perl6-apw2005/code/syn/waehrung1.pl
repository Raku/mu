#=waehrung1.pl - Perl 5 Version
use utf8;

# sub £to€ {  @_[0] * 1.4656 } # geht leider nicht!
sub PoundToEuro { @_[0] * 1.4656 }

# interpolierter Funktions-Aufruf
print "100£ sind @{[PoundToEuro(100)]}€\n";
# gibt '100£ sind 146.56€' aus
