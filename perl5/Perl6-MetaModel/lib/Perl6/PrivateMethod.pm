
package Perl6::PrivateMethod;

use strict;
use warnings;

use Carp 'confess';

use base 'Perl6::Method';

sub new {
    my ($class, $associated_with, $code) = @_;
    my $self = $class->SUPER::new($associated_with, $code);
    my $old = $self->{code};
    $self->{code} = sub {
        my ($self, @args) = @_;  
        (::CLASS() eq $self->associated_with)
            || confess "Cannot call private method from different class";
        $old->($self, @args); 
    };
    return $self;     
}

1;

__END__

=pod

=head1 NAME

Perl6::PrivateMethod - Private methods in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 SUPERCLASS

=over 4

=item I<Perl6::Method>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
