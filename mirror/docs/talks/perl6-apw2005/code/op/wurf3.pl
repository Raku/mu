#=Perl 5:
my $anz = 0;

for(1..3) {
    $anz++ if wurf() == 6;
}
print "okay!\n" if $anz == 1;
#=Perl 6:
if one({wurf()} xx 3) == 6 {
	"okay!".say;
}
