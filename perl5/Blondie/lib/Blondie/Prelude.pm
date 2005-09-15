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

Blondie::Prelude - 

=head1 SYNOPSIS

    use Blondie::Prelude;

=head1 DESCRIPTION

=cut


