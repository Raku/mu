#!/usr/bin/perl

package Blondie::Reducer::DuplicateFinder;
use base qw/Blondie::Reducer/;

use strict;
use warnings;

use Tie::RefHash;

sub new {
    my $class = shift;


    bless {
        seen => undef,
    }, $class;
}

sub duplicate_nodes {
    my $self = shift;
    my $program = shift;

    $self->{seen} = { };

    $self->reduce($program);

    grep { $self->{seen}{$_} > 1 } @{ $self->{nodes} };
}

sub generic_reduce {
    my $self = shift;
    my $node = shift;

    return $node if $self->{seen}{$node}++;

    push @{ $self->{nodes} }, $node;

    $self->SUPER::generic_reduce($node);
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Reducer::DuplicateFinder - find duplicate nodes in a program.

=head1 SYNOPSIS

    use Blondie::Reducer::DuplicateFinder;

    my $dup_finder = Blondie::Reducer::DuplicateFinder->new;

    my @nodes = $dup_finder->duplicate_nodes($program);

=head1 DESCRIPTION

Useful as a first pass in the code emission process, in order to compile all
reused nodes into their own emitter specific symbols.

=cut


