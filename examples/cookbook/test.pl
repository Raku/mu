#!/usr/bin/perl -w
use strict;


# this test script will compare the result of all the listed cookbook files
# to some expected set of results
# later it can be modifed to be somehow incorporated in the test suit

use File::Compare <compare>;


my $pugs = "../../pugs";

foreach my $p6 (<01strings/*.pl>) {
    (my $f = $p6) =~ s/.pl$//;
    my ($dir, $file) = split /\//, $f;
    next if not -e "$dir/$file.expected";
    unlink "$file.out";
    system "$pugs $f.pl > $file.out";
    if (compare("$file.out", "$dir/$file.expected")) {
        print "$dir/$file.pl has some difference\n";
    } else {
        print "ok $dir/$file.pl\n";
    }
}

