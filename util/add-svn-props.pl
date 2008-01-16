#!/usr/bin/perl
#
# Sets the usual svn properties on files.
#

use strict;
use warnings;

my $BINARY = 'svk';
my $failures = 0;

# Default properties to be applied
my %props = (
    'svn:mime-type' => 'text/plain; charset=UTF-8',
    'svn:eol-style' => 'native',
);

if (-d '.svn')
{
    $BINARY = 'svn';
}

if(! @ARGV)
{
    print STDERR "Usage: $0 file1 file2...\n       # Sets the usual svn properties on the given files.";
    exit 1;
}

for my $file (@ARGV)
{
    while(my($p, $v) = each %props)
    {
        my $exit_code = system($BINARY, 'propset', $p, $v, $file);

        if($exit_code != 0)
        {
            print STDERR "Failed setting property '$p' to '$v' for file '$file'\n";
            $failures++;
        }
    }
}

exit $failures;
