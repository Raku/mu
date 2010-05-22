#!/usr/bin/env perl
use 5.010;
use strict;
use warnings;

use Pod::Html;
use FindBin;
use File::Copy;
use File::Find;
use File::Spec;
use autodie;
use HTML::Template;

use vars qw/*name/;
*name   = *File::Find::name;

my $output_dir = shift(@ARGV) // '.';

my $input_dir = "$FindBin::Bin/../documentation";

for (qw(style.css)) {
    copy("$FindBin::Bin/../web/$_", $output_dir) or warn "Can't copy $_: $!";
}

my @docs;  
my %files;
find({wanted => \&pods_wanted}, $input_dir);

sub pods_wanted 
{
    push(@docs, $name) if $name=~/^.*\.pod\z/s;
}
my $outputdirname = File::Spec->rel2abs($output_dir);

# get the initial html markup out of the way... 
my $template = HTML::Template->new(filename => 'index.tmpl');
chdir $input_dir;


for my $pod (@docs) {
    my $filename = File::Spec->splitpath($pod);
    my $outfilename = (split(/\./, $filename))[0];
    $files{$outfilename} = "$outfilename.html";
    pod2html(
        "--infile=$pod",
        "--outfile=$outputdirname/$outfilename.html",
        "--podroot=/$input_dir",
        "--htmlroot=/",
#        "--libpods=" . join(':', @docs),
        "--css=/style.css",
        "--htmldir=$output_dir",
    );
}
open(OUTHTML, "+>", "$outputdirname/index.html") || die "Cannot open index.html for writing!, $!";

# here I come parsing for output files... 
my @file_list;
for my $key (sort keys %files) {
    push @file_list, { file => $files{$key}, fname => $key }
}

$template->param(file_list => \@file_list);
print OUTHTML $template->output;
close OUTHTML;
