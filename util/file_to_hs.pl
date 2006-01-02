#!/usr/bin/perl
use IPC::Open2;
use File::Basename;
use FindBin qw<$Bin>;

# XXX - This is not at all portable.

my ($in, $out) = @ARGV;

my ($name, $path) = fileparse($in);
$name =~ s/\..*$//;
my @path = grep {!/^src$|^$/} File::Spec->splitdir( $path );
my $fullname = join '.', @path, $name;
my $data = uc("__${name}__");

open OUT, "> $out" or die $!;
print OUT "module $fullname ($data) where\n\n";
print OUT "$data :: String\n";
print OUT "$data = \"";
dump_file($in);
print OUT "\"\n";

sub dump_file {
    my $file = shift;

    local *IN;
    open IN, "< $file" or die "Cannot open $file: $!";
    while (<IN>) {
        if (/^#include\s+['"<]?([^'">]+)/) {
            dump_file(dirname($file) . "/$1");
            next;
        }
        s/\\/\\\\/g; s/\n/\\n/g; s/\t/\\t/g; s/\r/\\r/g; s/\"/\\"/g;
        print OUT $_;
    }
}
