#!/usr/bin/env perl

# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# Most of the contet of this program is now in the Text::SmartLinks
# module in the Text-SmartLinks/ subdiectory.
# This script is here to provide the necessary flags and defaults required
# in the Pugs SVN.

use strict;
use warnings;
use FindBin;

my $version = `cat $ARGV[-1]/.revision`;
# The directory where the Synopses live.
# Please don't set syn-dir to elsewhere unless you have a good reason.
$ENV{PUGS_SMARTLINKS} = 1;
system qq($^X -I $FindBin::Bin/Text-SmartLinks/lib $FindBin::Bin/Text-SmartLinks/script/smartlinks.pl @ARGV --version $version);

