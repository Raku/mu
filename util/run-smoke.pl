#!/usr/bin/perl

use strict;
use warnings;
use Shell qw(svn make);

#
# run-smoke.pl /some/sandbox/dir /some/www/file.html
#
my $pugs_sandbox    = $ARGV[0] or die "Need pugs sandbox location";
my $html_location   = $ARGV[1] or die "Need HTML output file location";;

chdir($pugs_sandbox) or die "Could change directory: $!";

$ENV{HARNESS_PERL}  = "./pugs";
$ENV{PERL6LIB}	    = "ext/Test/lib";

my $output = svn("up") or die "Could not update pugs tree: $!";
$output   .= make() or die "Could not make pugs: $!";
system("perl ./util/yaml_harness.pl > /dev/null") == 0 or die "Could not run yaml harness: $!";
system("perl ./util/testgraph.pl > $html_location") == 0 or die "Could not convert .yml to testgraph: $!";

# END
