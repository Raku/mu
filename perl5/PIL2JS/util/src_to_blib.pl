#!/usr/bin/perl
#
# Copy all of the source, header, object and intermediate files in `src`
# to the blib6 archlib, so that it will all get installed. This is
# needed to be able to build Haskell based Perl6 extensions outside of
# the pugs tree.
#
use File::Copy qw(copy);
use File::Path qw(mkpath);

{
    copy_all("lib6/", 'blib6/lib/');
	copy_all("libjs/", 'blibjs/lib');
}

sub copy_all {
    my ($src, $dest) = @_;
    mkpath($dest);
    local *DIR;
    opendir(DIR, $src) or die $!;
    my @nodes = readdir(DIR);
    foreach my $node (sort @nodes) {
        next if $node =~ /^(\.|\.\.|\.svn)$/;
        my $src_path = "$src/$node";
        my $dest_path = "$dest/$node";
        if (-f $src_path) {
            copy($src_path, $dest_path);
        }
        if (-d $src_path) {
            copy_all($src_path, $dest_path);
        }
    }
}
