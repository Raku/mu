#!/usr/bin/perl

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
                print STDERR "// sm0p code still not being processed.\n";
                print STDERR $sm0p_code;
                next PRINCIPAL;
            };
        }
    }
    print {$output} $_;
}
