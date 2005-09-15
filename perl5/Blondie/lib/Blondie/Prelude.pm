#!/usr/bin/perl

package Blondie::Prelude;

use strict;
use warnings;

use Blondie::ADTs;

use Memoize;
memoize('env');

use Module::Pluggable (
    sub_name => "submodules",
    search_path => [__PACKAGE__],
    require => 1,
);

sub symbols {
    my $class = shift;
    map { $_->symbols } $class->submodules;
}

sub env {
    my $class = shift;
    Blondie::Env->new(
        $class->symbols,
    )
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Prelude - Aggregator of builtin functions for Blondie runtimes

=head1 SYNOPSIS

    use Blondie::Prelude;

=head1 DESCRIPTION

The prelude is essentially a reference implementation of all the things the
language must provide, including stuff for what cannot have a reference
implementation but is builtin anyway.

=cut


