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
chdir $input_dir;


for my $pod (@docs) {
    my $filename = File::Spec->splitpath($pod);
    my $outfilename = (split(/\./, $filename))[0];
    $files{$outfilename} = "$outfilename.html";
    copy($pod, $outputdirname) or die "Coudn't copy $pod!, $!\n";
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

# get the initial html markup out of the way... 
my $sometitle=" u4x Index ";
print OUTHTML << "CHUNK_OF_XHTML";
<?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head> 
<title>$sometitle</title> 
<link rel="stylesheet" type="text/css" href="style.css"/>
<meta http-equiv="content-type" content="text/html;charset=utf-8" /> 
</head> 
<body> 
<ul> 
CHUNK_OF_XHTML


# here I come parsing for output files... 
for my $key (sort keys %files) {
    print OUTHTML "<li><a href=\"$files{$key}\">$key</a></li>";
}

print OUTHTML <<"BOTTOM_CHUNK";
</ul>
</body>
</html>
BOTTOM_CHUNK
close OUTHTML;
