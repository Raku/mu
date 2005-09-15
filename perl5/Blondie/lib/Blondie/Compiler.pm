#!/usr/bin/perl

package Blondie::Compiler;
use base qw/Blondie::Reducer Blondie::Reducer::DynamicScoping Class::Accessor/;

use Scalar::Util qw/blessed/;

use Blondie::Nodes ();

use strict;
use warnings;

BEGIN {
    __PACKAGE__->mk_accessors(qw/runtime cache env/)
}

{
    package Blondie::Compiler::Placeholder;
    use base qw/Blondie::Val/;
    sub set { ${ $_[0] } = $_[1]; bless $_[0], "Blondie::Val" }
}

sub reduce {
    my $self = shift;
    my $node = shift;

    if (blessed($node)) {
        my $r = $self->runtime;
        if (my $native = $r->provides($node->digest)) {
            return $native;
        }
    }

    return $self->SUPER::reduce($node);
}

sub new {
    my $class = shift;
    bless {
    }, $class;
}

sub compile {
    my $self = shift;

    $self->runtime(shift);
    $self->env(my $env = shift);

    my $ast = shift;
    $self->cache({});

    $self->reduce($ast);
}

sub reduce_stub {
    my $self = shift;
    die "trying to reduce a stub";
}

sub reduce_param {
    my $self = shift;
    my $param = shift;

    $self->new_pad($param->val => undef);

    return $param;
}

sub reduce_thunk {
    my $self = shift;
    my $thunk = shift;

    $self->enter_scope;

    my $node = $self->generic_reduce($thunk);

    $self->leave_scope;

    return $node;
}

sub reduce_sym {
    my $self = shift;
    my $node = shift;
    my $symbol = $node->val;

    # if the symbol is a lexical then it can't be prebound
    eval { $self->find_dyn_sym($symbol) };
    return $node if not $@;

    $self->compile_time_resolution($symbol);
}

sub compile_time_resolution {
    my $self = shift;
    my $symbol = shift;

    return $self->{cache}{$symbol} if exists $self->{cache}{$symbol};

    my $val = $self->{cache}{$symbol} = Blondie::Compiler::Placeholder->new(undef);

    # if the symbol is not a predefined global it's an error. If it exists, it can be reduced to it's value
    my $builtin = $self->env->get($symbol) || die $@;

    # fill the place holder with the compiled version of the builtin
    $val->set($self->reduce($builtin));
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Compiler - 

=head1 SYNOPSIS

    use Blondie::Compiler;

=head1 DESCRIPTION

=cut


