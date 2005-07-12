
package Perl6::Container::Scalar;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

sub TIESCALAR {
    my ($class, $properties) = @_;
    $properties ||= {};
    # set some defaults
    my $perl6_scalar = bless {
        'value'      => undef,
        'properties' => $properties
    } => $class;
    $perl6_scalar->STORE($properties->{default}) 
        if exists $properties->{default};        
    return $perl6_scalar;
}

sub FETCH {
    my ($self) = @_;
    return $self->{value};
}

sub STORE {
    my ($self, $value) = @_;    
    if (defined $self->{properties}->{type}) {
        if (blessed($value)) {
            (
                ($value->isa($self->{properties}->{type}) || $self->{properties}->{type}->isa(blessed($value)))
                || 
                ($value->can('does') && $value->does($self->{properties}->{type}))
            ) || confess "Incorrect Type for Scalar (should be '$self->{properties}->{type}', but got '$value')";
        }
        else {
            confess "We can only handle blessed types currently (should be '$self->{properties}->{type}', but got '$value')";
        }
    }
    $self->{value} = $value;
}

sub DESTROY {
    my ($self) = @_;    
    undef $self;
}

1;

__END__

=pod

=head1 NAME

Perl6::Container::Scalar - A implementation of the Perl6 Scalar container

=head1 DESCRIPTION

This is a placeholder for a real Perl6::Scalar. This will likely eventually 
become the role(Scalar) in real Perl6, but for now it is just useful to me
in the metamodel :)

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut