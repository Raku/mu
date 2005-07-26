#!/usr/bin/perl -w
use strict;


# this test script will compare the result of all the listed cookbook files
# to some expected set of results
# later it can be modifed to be somehow incorporated in the test suit

use File::Compare qw(compare);


my $pugs = "../../pugs";

foreach my $p6 ("01strings/01-00introduction.p6") {
	(my $f = $p6) =~ s/.p6$//;
	my ($dir, $file) = split /\//, $f;
	return if not -e "$dir/$file.expected";
	unlink "$file.out";
	system "$pugs $f.p6 > $file.out";
	if (compare("$file.out", "$dir/$file.expected")) {
		print "$dir/$file.p6 has some difference\n";
	} else {
		print "ok $dir/$file.p6\n";
	}
}

