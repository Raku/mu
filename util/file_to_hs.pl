#!/usr/bin/perl
use IPC::Open2;
use File::Basename;
use FindBin qw<$Bin>;

# XXX - This is not at all portable.

my ($in, $out) = @ARGV;

my ($name, $path) = fileparse($in, '.pil');
my @path = grep {!/^src$|^$/} File::Spec->splitdir( $path );
my $fullname = join '.', @path, $name;
my $data = uc("__${name}__");

open IN, "< $in" or die $!;
open OUT, "> $out" or die $!;
print OUT "module $fullname ($data) where\n\n";
print OUT "$data :: String\n";
print OUT "$data = \"";
while (<IN>) {
    s/\\/\\\\/g; s/\n/\\n/g; s/\t/\\t/g; s/\r/\\r/g; s/\"/\\"/g;
    print OUT $_;
}
print OUT "\"\n";
