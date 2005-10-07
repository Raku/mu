#!/usr/bin/perl

package Blondie::Reducer;

use strict;
use warnings;

use UNIVERSAL::moniker;
use Scalar::Util ();

sub reduce {
    my $self = shift;
    my $node = shift;

	return $node unless $self->can_reduce($node);

	if (Scalar::Util::blessed($node) && (my $meth = $self->can("reduce_" . $node->moniker))) {
		return $self->$meth($node);
	} else {
		return $self->generic_reduce($node);
	}
}

sub can_reduce {
	my $self = shift;
	my $node = shift;;

	return unless Scalar::Util::blessed($node);
	return if $node->atomic;

	return 1;
}

sub generic_reduce {
    my $self = shift;
    my $node = shift;

	return $node unless Scalar::Util::blessed($node);

	$node->isa("Blondie::Node")
		or die "it doesn't look like $node is a piece of a Blondie program.";

   	$node->fmap(sub { $self->reduce($_[0]) });
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


