use strict;
BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

use Data::Dumper;
use File::Basename;
use YAML            qw[LoadFile Dump];

@ARGV or die usage();


for my $arg (@ARGV ) {
    
    my($key,$val) = split ':', $arg;
    die usage() if !$key or !$val;
   
   
    my $pat  = qr/$val/i;
    my @list = LoadFile( $Repoindex );

    for my $maybe ( @list ) {
        print $/. Dump( $maybe ) .$/ if $maybe->{$key} =~ $pat;
    }
}

sub usage {
    return qq[
    Usage: 
$0  key:value 

    key is one of the keys in the meta info
    value is a regex, compiled with qr//
    
    \n];
}
