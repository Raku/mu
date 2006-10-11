
package Perl6::MetaModel::Parser::Env;

use strict;
use warnings;

our $VERSION = '0.01';

sub new {
    my $class = shift;
    my $env = bless {
        _stack => []
    } => $class;
    $env->enter_scope;
    $env->set(@_) if @_;
    return $env;
}

sub enter_scope {
    my $self = shift;
    unshift @{$self->{_stack}} => {};
}

sub leave_scope {
    my $self = shift;        
    shift @{$self->{_stack}};
}

sub set {
    my ($self, %vars) = @_;
    @{($self->_current || return)}{keys %vars} = values %vars;
}

sub get {
    my ($self, $name) = @_;
    foreach my $depth (0 .. $self->_depth) {
        if (my $value = ($self->_current($depth) || return)->{$name}) {
            return $value;
        }
    }
}

sub DESTROY { (shift)->leave_scope }    

## private methods

sub _depth { scalar @{(shift)->{_stack}} }
sub _current { 
    my ($self, $depth) = @_;
    $depth ||= 0;
    $self->{_stack}->[$depth];
}

1;