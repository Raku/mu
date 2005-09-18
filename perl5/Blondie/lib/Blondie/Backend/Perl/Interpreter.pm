#!/usr/bin/perl

package Blondie::Backend::Perl::Interpreter;
use base qw/Blondie::Reducer Blondie::Reducer::DynamicScoping/;

use strict;
use warnings;
no warnings 'recursion';

sub new {
    my $class = shift;
    bless {
        params => undef,
        param_stack => [],
    }, $class;
}

sub can_reduce { 1 }

sub generic_reduce {
	my $self = shift;
	my $node = shift;
	die "generic reductions don't make sense while interpreting ($node)";
}

sub execute {
    my $self = shift;
    my $prog = shift;
    $self->reduce($prog);
}

sub reduce_val { $_[1]->val }

sub reduce_app {
    my $self = shift;
    my $app = shift;

    my ($thunk, @params) = map { $self->reduce($_) } $app->values;

    $self->prepare_scope(@params);

    # profiling actually slows this down by around 30%
    #my $t = times;
    #my $v = 
    $self->reduce($thunk);
    #my $t2 = times;
    #$self->{prof}{$thunk} += $t2-$t;
    #$v;
}

sub reduce_thunk {
    my $self = shift;
    my $thunk = shift;

    # FIXME - in the future the compiler might allow adding globals
    # ought to check if the runtime has a hash for this right now this is a non
    # issue since even higher order functions get prims instead of thunks when
    # possible

    my $body = $thunk->val;

    $self->enter_scope;
    
    my $v = $self->reduce($body);

    $self->leave_scope;

    return $v;
}

sub reduce_param {
    my $self = shift;
    my $param = shift;

    $self->new_pad($param->val => $self->shift_param);
}

sub reduce_prim {
    my $self = shift;
    my $prim = shift;

    $self->enter_scope;
    my @params = $self->params;

    # the body is a code ref
    my $v = $prim->body->($self, map { $self->shift_param } 1 .. $prim->arity);

    $self->leave_scope;

    return $v;
}

sub reduce_sym {
    my $self = shift;
    my $symbol = shift->val;

    $self->find_dyn_sym($symbol)->val;
}

sub reduce_seq {
    my $self = shift;
    my $seq = shift;
    (map { $self->reduce($_) } $seq->values)[-1];
}

sub leave_scope {
    my $self = shift;

    $self->SUPER::leave_scope(@_);
    
#    warn "unbound params @{ $self->{params} }" if $self->{params} and @{ $self->{params} };
    $self->{params} = pop @{ $self->{param_stack} };
}

sub prepare_scope {
    my $self = shift;
    push @{ $self->{param_stack} }, $self->{params};
    $self->{params} = [ @_ ];
}

sub shift_param {
    my $self = shift;
    shift @{ $self->{params} };
}

sub params {
    my $self = shift;
    @{ $self->{params} };
}

__PACKAGE__;

__END__

=pod

=head1 NAME

Blondie::Backend::Perl::Interpreter - A perl interpreter for compiled Blondie programs.

=head1 SYNOPSIS

    use Blondie::Backend::Perl::Interpreter;

=head1 DESCRIPTION

This is a subclass of L<Blondie::Reducere> that performs the actions denoted by
the AST, ultimately flattenning the tree down to a single value.

=cut


