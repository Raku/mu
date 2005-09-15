#!/usr/bin/perl

package Blondie::Runtime;

use strict;
use warnings;

use UNIVERSAL::require;

use Blondie::Prelude;

sub new {
    my $class = shift;

    bless {

    }, $class;
}

sub compiler {
    my $self = shift;

    my $class = $self->compiler_class;

    $class->require;
    $class->new;
}

sub compile {
    my $self = shift;
    my $prog = shift;

    $self->compiler->compile($self, Blondie::Prelude->env, $prog);
}

sub compiler_class { "Blondie::Compiler" }

sub provides {
    die "virtual method provides called on @_";
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Runtime - Base class for execution system (compiler + interpreter or equivalent).

=head1 SYNOPSIS

    use Blondie::Backend::Foo; # is-a Blondie::Runtime

    my $compiled = Blondie::Backend::Foo->compile($program);
    my $result = Blondie::Backend::Foo->execute($compiled);

    my $same_result = Blondie::Backend::Foo->run($program);

=head1 DESCRIPTION

The runtime object is responsible for providing the compiler with replacement
data for stubs and other thingies.

It also gives the compiler the default environment to work with (from
L<Blondie::Prelude>), and provides an easy interface for executing ASTs.

=cut


