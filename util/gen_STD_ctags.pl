#!/usr/bin/perl
use warnings;
use strict;
my %tag;
while (<>) {
    if (/^\s*(regex|rule|token|sub|method|proto token) \s+ (\w+) \s/x) {
        $tag{$2} = $.;
    }
}
for (sort {$a cmp $b} keys %tag) {
    print "$_\tSTD.pm\t$tag{$_}\n";
}

