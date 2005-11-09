
package Perl6::Core::Closure;

use Perl6::Core::Type;
use Perl6::Core::Num;
use Perl6::Core::Ref;
use Perl6::Core::Symbol;
use Perl6::Core::List;

package closure;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my ($class, $env, $signature, $body) = @_;
    (blessed($env) && $env->isa('closure::env'))
        || confess "env must be a closure::env";
    # params is a hash whose keys are the names
    # of variables, and whose values are the 
    # expected types of those objects (or possibly
    # the default values too)
    (blessed($signature) && ($signature->isa('closure::params') || $signature->isa('closure::signature')))
        || confess "signature must be an closure::params or closure::signature type";
    (ref($body) eq 'CODE')
        || confess "body must be a code ref";
    
    if ($signature->isa('closure::params')) {
        $signature = closure::signature->new(
            params  => $signature,
            returns => 'type'
        );
    }    
    
    my $local_env = closure::env->new();
    $local_env->next($env);
    bless {
        sig  => $signature,
        body => $body,
        env  => $local_env
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
    my $return_value = $self->{body}->($self->{env});
    (blessed($return_value) && ($return_value->isa($self->{sig}->returns) || $return_value->isa('nil')))
        || confess "bad return value, got($return_value) expected(" . $self->{sig}->returns . ")";
    return $return_value;
} 

sub _bind_params {
    my ($self, $args) = @_;
    (blessed($args) && $args->isa('list'))
        || confess "Args must be a list";
    my $params = $self->{sig}->params;
    ($args->length()->less_than_or_equal_to($params->length) == $bit::TRUE)
        || confess "too many arguments passed to closure got(" . $args->length()->to_native . ") expected(" . $params->length->to_native . ")";      
    # loop through the param keys
    for my $i (0 .. $params->elems->to_native) {
        my $value; 
        my $param      = $params->fetch(num->new($i));
        my $param_type = $param->type->to_native;
        my $arg        = $args->fetch(num->new($i));
        unless ($arg->isa('nil') && $param->to_str->to_native =~ /^\?/) {
            (blessed($arg) && $arg->isa($param_type))
                || confess "got the wrong type for " . $param->to_str->to_native . " got($arg) -> expected($param_type)"
                    if $param_type ne '';
        }
        $self->{env}->set($param->to_str->to_native, $arg);
    }
}

package closure::signature;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

sub new {
    my ($class, %options) = @_;
    (exists $options{params} && blessed($options{params}) && $options{params}->isa('closure::params'))
        || confess "Bad params in signature";
    $options{returns} = 'type' if !(exists $options{returns});
    bless { %options } => $class;
}

sub params  { (shift)->{params}  }
sub returns { (shift)->{returns} }

package closure::params;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'list';

sub new {
    my ($class, @values) = @_;
    (blessed($_) && $_->isa('symbol'))
        || confess "You must store a symbol type in a param list"
            foreach @values;
    $class->SUPER::new(@values);
}

sub store {
    my ($self, $index, $value) = @_;
    (blessed($value) && $value->isa('symbol'))
        || confess "You must store a symbol type in a param list";
    $self->SUPER::store($index, $value);
}

sub unshift : method { 
    my $self = shift;
    (blessed($_) && $_->isa('symbol'))
        || confess "you can only add symbol types to a param list"
            foreach @_;
    $self->SUPER::unshift(@_);
}

sub push : method { 
    my $self = shift;
    (blessed($_) && $_->isa('symbol'))
        || confess "you can only add symbol types to a param list"
            foreach @_;
    $self->SUPER::push(@_);
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