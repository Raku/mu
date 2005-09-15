#!/usr/bin/perl

package Blondie::Reducer::DynamicScoping;

use strict;
use warnings;

{
    use Blondie::ADTs;
    package Blondie::Reducer::DynamicScoping::Pad;
    use base qw/Blondie::Map/;
}

sub find_dyn_sym {
    my $self = shift;
    my $symbol = shift;

    foreach my $pad ($self->pads) {
    return $pad if $pad->name eq $symbol;
    }

    die "symbol $symbol could not be resolved by $self";
}

sub new_pad {
    my $self = shift;
    
    my $name = shift;
    my $val = shift;

    my $pad = Blondie::Reducer::DynamicScoping::Pad->new(
        name => $name,
        val => $val,
    );

    push @{ $self->{scopes}[0] }, $pad;
}

sub pads {
    my $self = shift;
    map { @$_ } $self->scopes;
}

sub scopes {
    my $self = shift;
    @{ $self->{scopes} }
}

sub enter_scope {
    my $self = shift;
    unshift @{ $self->{scopes} }, [];
}

sub leave_scope {
    my $self = shift;
    shift @{ $self->{scopes} };
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Reducer::DynamicScoping - a base class (probably a role) for
implementing dynamic scopes (not really a reduer).

=head1 SYNOPSIS

    use base qw/Blondie::Reducer::DynamicScoping/;

    ...

    $self->enter_scope; # opens a new scope

    $self->new_pad("name" => $value); # shadows any previous "name"

    $self->find_dyn_sym("name"); # returns $value

    $self->leave_scope; # deletes pads since last enter_scope

=head1 DESCRIPTION

Assumes a hash structure of $self, and uses the 'scopes' keys therein.

=cut


