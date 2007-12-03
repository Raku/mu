#!/usr/bin/perl
use strict;
use vars qw($count $count_eval);
my $a = qr/a/;
$count_eval= qr/(??{eval '$a'})(?{print "?";})(??{$count_eval})/;
$count= qr/(??{$a})(?{print "!";})(??{$count})/;
"aaaa" =~ /^$count$/;
print "\n";
"aaaa" =~ /^$count_eval$/;
print "\n";
