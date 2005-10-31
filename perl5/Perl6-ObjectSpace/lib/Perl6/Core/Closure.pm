
package Perl6::Core::Closure;

use Perl6::Core::Type;
use Perl6::Core::Num;
use Perl6::Core::Ref;

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
    (blessed($params) && $params->isa('list'))
        || confess "params must be an list type";
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

# we dont really have a native form
sub to_native { shift }

# conversion to other native types
sub to_num { num->new((shift) + 0)  }
sub to_str { str->new((shift) . '') }
sub to_bit { bit->new(1)            }

# methods 

sub do {
    my ($self, $args) = @_;
    $args ||= list->new();
    $self->_bind_params($args);
    $self->{body}->($self->{env});
} 

sub _bind_params {
    my ($self, $args) = @_;
    (blessed($args) && $args->isa('list'))
        || confess "Args must be a list";
    # loop through the param keys
    for my $i (0 .. $self->{params}->elems->to_native) {
        my $value; 
        $self->{env}->set(
            $self->{params}->fetch(num->new($i))->to_native,
            $args->fetch(num->new($i))
            );
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

sub create {
    my ($self, $name, $value) = @_;
    (defined $name) 
        || confess "You must supply a name to set";
    (blessed($value) && $value->isa('type'))
        || confess "You must set '$name' to a native type";
    $self->{env}->{$name} = reference->new($value);      
}

sub set {
    my ($self, $name, $value) = @_;
    (defined $name) 
        || confess "You must supply a name to set";
    (blessed($value) && $value->isa('type'))
        || confess "You must set '$name' to a native type";
    my $current = $self;
    while ($current) {
        if (exists $current->{env}->{$name}) {
            $current->{env}->{$name}->store($value);
            return;
        }
        $current = $current->next;
    }
    $self->create($name, $value);
}

sub get {
    my ($self, $name) = @_;
    (defined $name) 
        || confess "You must supply a name to get";    
    my $current = $self;
    while ($current) {
        return $current->{env}->{$name}->fetch
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