#!/usr/bin/perl -nw

use strict;
use File::Spec::Functions;

use Fatal qw/open close/;

BEGIN { $/ = "### SPLIT HERE ### " } # per section

if (2 .. 1==1){ # skip first paragraph 
	my ($dirname, $pod) = split " ###\n\n"; # finish the splitting
	
	for ($pod){ # munge the pod a bit
		s/=item (.*)/=head1 DIRECTORY\n\n$1\n\n=head1 TEST FILES/; # first occurance

		s/=back\n\n=head1 COPYRIGHT.*$//s; # this applies only if it's the last section
	}

	my $target = catfile($dirname, "test_catalog.pod");
	die "$target exists, won't overwrite" if -f $target;
	open POD, ">$targer"; # FIXME noclobber
	print POD $pod;
	close POD;
}

=kwid

= NAME

split_test_catalog.pl - A program to make test_catalog.pod files in each test
dir, based on the output of [file://catalog_tests.pl].

= DESCRIPTION

The output of [file://catalog_tests.pl] is about 100k, right now. This program
splits that output into many smaller files, in order to make it easier to find
the specifications tested.

= AUTHORS

* Stevan Little (STEVAN)
* Yuval Kogman (NUFFIN)
* Brian Ingerson (INGY)
* Zohar Kelrich
* James Mastros (JMASTROS)

= COPYRIGHT

Copyright 2005 by Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>.

This code is free software; you can redistribute it and/or modify it under
the terms of either:

    a) the GNU General Public License, version 2, or
    b) the Artistic License, version 2.0beta5 or any later version.

For the full license text, please see the F<GPL-2> and F<Artistic-2> files
under the F<LICENSE> directory in the Pugs distribution.

=cut
