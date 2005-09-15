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

Blondie::Runtime - 

=head1 SYNOPSIS

    use Blondie::Runtime;

=head1 DESCRIPTION

=cut


