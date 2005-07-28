
package Perl6::PrivateMethod;

use strict;
use warnings;

use base 'Perl6::Method';

sub call { 
    my ($self, @args) = @_;  
    ::CLASS();
    $self->SUPER::call(@args); 
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
