#!/usr/bin/perl -w
use strict;

for my $ext (qw( hi o)) {
    my @files=`find . -name \"*.$ext\"`;
    for my $f (@files) {
        chomp $f;
        system("rm -f $f");
    }
}
system("rm -f *~");

