
package Perl6::Core::Closure;

use Perl6::Core::Type;
use Perl6::Core::Num;
use Perl6::Core::Str;
use Perl6::Core::Bit;
use Perl6::Core::Closure;

package block;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my ($class, $env, $body) = @_;
    (blessed($env) && $env->isa('closure::env'))
        || confess "env must be a closure::env";
    (ref($body) eq 'CODE')
        || confess "body must be a code ref";
    my $local_env = closure::env->new();
    $local_env->next($env);
    bless {
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
sub env { (shift)->{env} }

sub do {
    my $self = shift;
    $self->{body}->($self->{env});
} 

sub do_while {
    my ($self, $condition) = @_;
    (blessed($condition) && $condition->isa('block'))
        || confess "Condition must be a block";
    while ($condition->do()->to_bit == $bit::TRUE) {
        $self->do();
    }
}

sub do_until {
    my ($self, $condition) = @_;
    (blessed($condition) && $condition->isa('block'))
        || confess "Condition must be a block";
    while ($condition->do()->to_bit == $bit::FALSE) {
        $self->do();
    }
}

1;

__END__