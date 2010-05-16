#!/usr/bin/env perl
use 5.010;
use strict;
use warnings;

use Pod::Html;
use FindBin;
use File::Copy;
use File::Find;
use File::Basename;
use autodie;

use vars qw/*name/;
*name   = *File::Find::name;

my $output_dir = shift(@ARGV) // '.';

my $input_dir = "$FindBin::Bin/../documentation";

for (qw(style.css)) {
    copy("$FindBin::Bin/../scripts/$_", $output_dir) or warn "Can't copy $_: $!";
}

my @docs;  
find({wanted => \&pods_wanted}, $input_dir);

sub pods_wanted 
{
    push(@docs, $name) if $name=~/^.*\.pod\z/s;
}
chdir $input_dir;


for my $pod (@docs) {
    my $outfilename=fileparse($pod, qr/\Q.pod\E/);
    my $dirname=dirname($pod);
    pod2html(
        "--infile=$pod",
        "--outfile=$dirname/$outfilename.html",
        "--podroot=/$input_dir",
        "--htmlroot=/",
#        "--libpods=" . join(':', @docs),
        "--css=$input_dir/style.css",
        "--htmldir=$output_dir",
    );
}

