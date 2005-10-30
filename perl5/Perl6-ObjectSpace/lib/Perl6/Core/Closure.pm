
package Perl6::Core::Closure;

use Perl6::Core::Type;
use Perl6::Core::Num;

package closure;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my ($class, $env, $params, $body) = @_;
    (blessed($env) && $env->isa('closure::env'))
        || confess "env must be a closure::env";
    # params is a hash whose keys are the names
    # of variables, and whose values are the 
    # expected types of those objects (or possibly
    # the default values too)
    (blessed($params) && $params->isa('hash'))
        || confess "params must be an hash type";
    (ref($body) eq 'CODE')
        || confess "body must be a code ref";
    my $local_env = closure::env->new();
    $local_env->next($env);
    bless {
        params => $params,
        body   => $body,
        env    => $local_env
    } => $class;
}

sub do {
    my ($self, $args) = @_;
    $self->_bind_params($args);
    $self->{body}->($self->{env});
} 

sub _bind_params {
    my ($self, $args) = @_;
    (blessed($args) && $args->isa('hash'))
        || confess "Args must be a hash";
    # loop through the param keys
    for my $param ($self->{params}->keys->to_native) {
        my $value; 
        # if we have an arg match, get it
        if ($args->exists($param)->to_native) {
            $value = $args->fetch($param);            
        }
        # otherwise use the "default"
        # in the params
        else {
            $value = $self->{params}->fetch($param);
        }
        # NOTE: 
        # we need to convert the 
        # param to a native string
        $self->{env}->set($param->to_native => $value);
    }
}

package closure::env;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my $class = shift;
    bless {
        env  => {},
        next => undef
    } => $class;
}

sub set {
    my ($self, $name, $value) = @_;
    (defined $name) 
        || confess "You must supply a name to set";
    (blessed($value) && $value->isa('type'))
        || confess "You must set '$name' to a native type";
    $self->{env}->{$name} = $value;  
}

sub get {
    my ($self, $name) = @_;
    (defined $name) 
        || confess "You must supply a name to get";    
    my $current = $self;
    while ($current) {
        return $current->{env}->{$name} 
            if exists $current->{env}->{$name};  
        $current = $current->next;
    }
    confess "'$name' not found in dynamic env";
}

sub next {
    my ($self, $env) = @_;
    $self->{next} = $env if defined $env;
    $self->{next};
}

1;

__END__

=pod

=head1 NAME

closure - the core closure type

=cut