
package Perl6::Core::Hash;

use Perl6::Core::Type;
use Perl6::Core::Num;
use Perl6::Core::Str;
use Perl6::Core::Bit;
use Perl6::Core::Nil;
use Perl6::Core::List;

package hash;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my ($class, @values) = @_;
    (scalar(@values) % 2 == 0)
        || confess "not enough elements to construct a hash (must be an even number)";
    my %values;
    while (my ($key, $value) = splice(@values, 0, 2)) {
        ((blessed($key)   && $key->isa('type')) && 
         (blessed($value) && $value->isa('type')))
            || confess "Keys must be strings, and values must be native types";
        $values{$key->to_str->to_native()} = $value;
    }
    bless \%values => $class;
}

# native conversion
sub to_native { %{(shift)} }

# conversion to other types
sub to_str { str->new('' . (shift)->to_native) }
sub to_num { (shift)->keys->length             }
sub to_bit { (shift)->keys->length->to_bit     }

# methods ...

sub fetch {
    my ($self, $key) = @_;
    (blessed($key) && $key->isa('type'))
        || confess "Key must be a str type";
    return $self->{$key->to_str->to_native};
}

sub store {
    my ($self, $key, $value) = @_;
    (blessed($key) && $key->isa('type'))
        || confess "Key must be a str type";
    (blessed($value) && $value->isa('type'))
        || confess "Index must be a native type";            
    $self->{$key->to_str->to_native} = $value;
    nil->new();    
}

sub remove {
    my ($self, $key) = @_;
    (blessed($key) && $key->isa('type'))
        || confess "Key must be a str type";
    delete $self->{$key->to_str->to_native};
    nil->new();
}

sub keys { 
    my %native = (shift)->to_native;
    list->new(map { str->new($_) } keys %native) 
}

sub values { 
    my %native = (shift)->to_native;
    list->new(values %native);
}

sub length { (shift)->keys->length }

sub exists {
    my ($self, $key) = @_;
    (blessed($key) && $key->isa('type'))
        || confess "Key must be a str type";
    bit->new(exists $self->{$key->to_str->to_native} ? 1 : 0);    
}

1;

__END__

=pod

=head1 NAME

hash - the core hash type

=cut