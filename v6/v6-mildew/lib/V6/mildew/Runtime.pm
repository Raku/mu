package V6::mildew::Runtime;
use strict;
use warnings;
use v5.10;
use SMOP::Embed;
use SMOP;
sub load {
    my ($perl6_so) = @_;
    say "$perl6_so";
    my $path = SMOP::ld_library_path;
    for my $module (SMOP::modules) {
        SMOP::Embed::load("$path/libsmop-$module.so");
    }
    $ENV{'LD_LIBRARY_PATH'} = join(':',SMOP::ld_library_path);
    SMOP::Embed::load_and_run($perl6_so);
}
1;
