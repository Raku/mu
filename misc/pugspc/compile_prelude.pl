#!/usr/bin/perl -w
use strict;

my @prelude = `./compile perl6/Prelude/PIR.pm`;

# post-compilation fixes for prelude

print ".namespace ['Main']\n";
my $skip=1;
for (@prelude) {

    /\.namespace\s+\[\'main\'\]/ && do {
        $skip=0;
        next;
    };
    next if $skip==1;
    /\.sub\s+\"\&\*END/ && do {$skip=1;next;};
    s/\&\*/\&/g;
    s/LABEL/prelude_LABEL/g;
    s/\s+\:outer\(\"MAIN\"\)//;
    print;
}
