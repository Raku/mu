
package Perl6::SubMethod;

use strict;
use warnings;

use Scalar::Util 'blessed';

use base 'Perl6::Method';

# XXX - this just bypasses the local call()
sub force_call { 
    my ($self, @args) = @_;  
    $self->SUPER::call(@args);     
}

sub call { 
    my ($self, @args) = @_;  
    # next METHOD if $?SELF != $?CLASS;
    return ::next_METHOD() if $args[0]->{class} ne $self->associated_with; 
    $self->SUPER::call(@args); 
}

1;

__END__

=pod

=head1 NAME

Perl6::SubMethod - Submethods in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 SUPERCLASS

=over 4

=item I<Perl6::Method>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
