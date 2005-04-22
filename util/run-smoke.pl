#!/usr/bin/perl

use strict;
use warnings;
use Shell qw(svn);
use Config;
use File::Spec;

#
# NOTE - this script is replaced by the 'make smoke' target,
# mainly to prevent spurious rebuilds.
#
# I'm not sure how it worked with sandboxes - the hardcoded paths
# look wrong to me anyway - but in case it did work, I'm not removing
# this file from the tree, just dropping the ref to it from Makefile.
#

#
# run-smoke.pl /some/sandbox/dir /some/www/file.html
#
my $pugs_sandbox    = $ARGV[0] or die "Need pugs sandbox location";
my $html_location   = $ARGV[1] or die "Need HTML output file location";;

chdir($pugs_sandbox) or die "Could change directory: $!";

$ENV{HARNESS_PERL}  = "./pugs";
$ENV{PERL6LIB}	    = "ext/Test/lib";

sub make { return `$Config{make} @_` };
my $dev_null = File::Spec->devnull;

my $output ;# = svn("up") or die "Could not update pugs tree: $!";
$output   .= make("optimized") or die "Could not make pugs: $!";
system("perl -w ./util/yaml_harness.pl") == 0 or die "Could not run yaml harness: $!";
system("perl -w ./util/testgraph.pl >$html_location") == 0 or die "Could not convert .yml to testgraph: $!";

# END
