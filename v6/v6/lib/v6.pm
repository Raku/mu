package v6;
# ABSTRACT: support running Perl6 codes with 'use v6-implementation';
use strict;
use warnings;
use v5.10;
sub import {
    my ($module,@args) = @_;
    my $submodule = $args[0];
    if ($submodule =~ /^-/) {
        $submodule =~ s/^-//;
        $submodule = 'v6::'.$submodule;
        eval("require $submodule");
        die if $@;
        $submodule->import(@args);
    }
}
1;
