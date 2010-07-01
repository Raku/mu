package SMOP;
# ABSTRACT: Simple Meta Object Programming
my @MODULES = qw(s0native dump nagc util capture interpreter mold yeast native lost s1p p6opaque s1p-oo p5 mold-message profile);
use File::ShareDir qw(dist_dir);
use strict;
use warnings;
our $VERSION = 0.01;
sub lib_flags {
    my @LIBS;
    push(@LIBS,'-L',dist_dir('SMOP'));
    for my $module (@MODULES) {
        push(@LIBS,'-lsmop-' . $module);
    }
    return @LIBS;
}
sub include_flags {
    return "-I".dist_dir('SMOP');
}
sub ld_library_path {
    dist_dir('SMOP');
}
1;
