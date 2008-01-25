#!/usr/bin/perl

use strict;
use warnings;

my ($in, $out) = @ARGV;

open my $input, '<', $in or die $!;
open my $output, '>', $out or die $!;

my $sm0p_code = '';
PRINCIPAL:
while (<$input>) {
    if (/q:sm0p/) {
        $sm0p_code = $_;
        while (<$input>) {
            $sm0p_code .= $_;
            if ( $_ =~ /\}/ ) {
                print {$output} preprocess($sm0p_code);
                next PRINCIPAL;
            };
        }
    }
    print {$output} $_;
}


sub preprocess {
    my $code = shift;
    print STDERR "sm0p.pl:$in:warning - sm0p code still not being processed.\n";
    return " /** sm0p code still not being processed \n".$code." */ \n";
}
