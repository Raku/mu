#!/usr/bin/perl

my ($in, $out) = @ARGV;

open my $input, '<', $in or die $!;
open my $output, '>', $out or die $!;

PRINCIPAL:
while (<$input>) {
    if (/q:sm0p/) {
        while (<$input>) {
            next PRINCIPAL if /\}/;
        }
    }
    print {$output} $_;
}
