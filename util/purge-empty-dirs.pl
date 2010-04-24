#!/usr/bin/perl

#
# This is written to be run from a Subversion checkout directory.
# It looks for empty directories and deletes them.
# It does NOT do a 'svn commit',
#       so you can 'svn revert -R .' to change your mind.
# I wrote this because when we delete things with git-svn,
# empty directories are sometimes left behind.
# Fixing that with the original svn client is the only way I know how.
#

use 5.010;
use strict;
use warnings;

# Go to the Subversion checkout directory
my $base = shift @ARGV // '.';
chdir $base or die "Can't chdir '$base': $!";
if ( !-d '.svn' ) {
    die "'$base' does not appear to be a Subversion checkout\n";
}

# Get a list of every directory, depth first, but not .svn stuff
open my $find_fh, '-|', 'find', '.', qw( -depth -type d -print )
    or die "Can't run find: $!";
my @directories = grep { ! m{ / \.svn (?: / | $ ) }xms } <$find_fh>;
close $find_fh or die "close pipe failed: $!";

my %gone;   # directories we decided to purge

DIRECTORY:
for my $dir ( @directories ) {
    chomp $dir;
    opendir my $dir_dh, $dir  or die "Can't opendir '$dir': $!";

    # Look for something that's not .svn and not a purge-able directory
    while ( defined( my $sub = readdir $dir_dh ) ) {
        next if $sub =~ /\A\.\.?\z/;
        next if $sub eq '.svn';
        next if $gone{"$dir/$sub"};
        next DIRECTORY;
    }
    closedir $dir_dh  or die "closedir failed: $!";

    # Couldn't find anything "real" in here, so bye!
    $gone{$dir}++;
    if ( 0 != system "svn rm $dir" ) {
        die "'svn rm' abended\n";
    }
}

say 'Now, "svn commit", if you dare.';
