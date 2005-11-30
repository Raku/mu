use strict;
use YAML            qw[LoadFile Dump];
use File::Basename  qw[basename];
use Data::Dumper;
use Cwd;

BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

my $alts = LoadFile( $Altfile );

for my $struct ( sort { $a->{package} cmp $b->{package} } 
                 LoadFile( $Available ) 
) {
    print "$struct->{package} -- $struct->{description}\n";

    ### XXX yaml this file?
    open my $fh, "$Metactrl/$struct->{package}/$Fileslist" or die $!;
    while ( <$fh> ) {
        chomp;
        s/^$Root/\$Root/;
        print "\t$_\n";
    }
 
    ### show alternatives
    if( my $aref = $alts->{ $struct->{package} }->{bin} ) {
        print "\t*** Primary alternative for:\n";
        for my $alt (@$aref) {
            print "\t\t$alt\n";
        }
    }
}


