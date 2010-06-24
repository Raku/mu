#!/usr/bin/env perl

# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# Most of the contet of this program is now in the Text::SmartLinks
# module in the Text-SmartLinks/ subdiectory.
# This script is here to provide the necessary flags and defaults required
# in the Pugs SVN.

use strict;
use warnings;
use FindBin;

my $version = get_pugs_rev();
# The directory where the Synopses live.
# Please don't set syn-dir to elsewhere unless you have a good reason.
$ENV{PUGS_SMARTLINKS} = 1;
system qq($^X -I $FindBin::Bin/Text-SmartLinks/lib $FindBin::Bin/Text-SmartLinks/script/smartlinks.pl @ARGV --version $version);

sub get_pugs_rev {
    my $stdout = `$^X $FindBin::Bin/version_h.pl`;
    my ($pugs_rev) = ($stdout =~ /^(\d+)/);
    if (!$pugs_rev) {
        die "Can't determine  version of the pugs repository via 'svn info'";
#        # if we don't have access to others' svk info
#        # (which is the case on feather where i'm using
#        # Audrey's pugs working copy), then parse pugs_version.h
#        # directly:
#        if (open my $in, "$FindBin::Bin/../src/Pugs/pugs_version.h") {
#            warn "reading pugs_version.h...\n";
#            local $/;
#            my $str = <$in>;
#            ($pugs_rev) = ($str =~ /PUGS_SVN_REVISION\s+(\d+)/);
#        }
    }
    return $pugs_rev;
}
