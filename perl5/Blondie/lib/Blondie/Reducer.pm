#!/usr/bin/perl

package Blondie::Reducer;

use strict;
use warnings;

use UNIVERSAL::moniker;
use Scalar::Util ();

sub reduce {
    my $self = shift;
    my $node = shift;

    if (Scalar::Util::blessed($node) and my $meth = $self->can("reduce_" . $node->moniker)) {
        return $self->$meth($node);
    } else {
        return $self->generic_reduce($node);
    }
}

sub generic_reduce {
    my $self = shift;
    my $node = shift;

    if (Scalar::Util::blessed($node)) {
        $node->isa("Blondie::Node")
            or Carp::croak "it doesn't look like $node is a piece of a Blondie program.";

        return $node->atomic ? $node : $node->fmap(sub { $self->reduce($_[0]) });
    } else {
        return $node;
    }
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Reducer - generic reducing base class for fmapping nodes.

=head1 SYNOPSIS

    package My::ASTWalker;

    use base qw/Blondie::Reducer/;

    sub reduce_sym {
        my $self = shift;
        my $sym_node = shift;

        my $symbol = $sym_node->val;

        $table{$symbol}; # find the symbol
    }

    # and then

    my $reduced = My::ASTWalker->new->reduce($program);

=head1 DESCRIPTION

=cut


