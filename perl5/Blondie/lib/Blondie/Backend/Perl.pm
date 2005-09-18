#!/usr/bin/perl

package Blondie::Backend::Perl;
use base qw/Blondie::Runtime/;

use strict;
use warnings;

use Blondie::Backend::Perl::Builtins;

use UNIVERSAL::require;

sub compiler_class { __PACKAGE__ . "::Compiler" };

sub interpreter_class { __PACKAGE__ . "::Interpreter" }

sub run {
    my $self = shift;

    my $c = $self->compile(@_);

    $self->execute($c);
}

sub interpreter {
    my $self = shift;
    my $class = $self->interpreter_class;
    $class->require or die $UNIVERSAL::require::ERROR;
    $class->new;
}

sub provides {
    my $self = shift;
	my $node = shift;

	warn Carp::longmess unless $node->can("digest");
    Blondie::Backend::Perl::Builtins->find($node->digest || return);
}

sub cast_node_type {
	my $self = shift;
	Blondie::Backend::Perl::Builtins->cast(@_);
}

sub execute {
    my $self = shift;
    my $prog = shift;

    $self->interpreter->execute($prog);
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::Perl - an interpreting backend for Blondie ASTs

=head1 SYNOPSIS

    use Blondie::Backend::Perl;

=head1 DESCRIPTION

L<Blondie::Backend::Perl::Interpreter> contains the tree reduction code.

L<Blondie::Backend::Perl::Builtins> contains the builtin operations and values
that replace prelude nodes.

=cut


