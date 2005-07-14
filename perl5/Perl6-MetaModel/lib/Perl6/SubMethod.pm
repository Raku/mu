
package Perl6::SubMethod;

use strict;
use warnings;

use Scalar::Util 'blessed';

use base 'Perl6::Method';

# XXX - this only checks that the invocant is 
# an instance of the correct class. This is 
# enough for now, but it should eventually
# also be able to check the calling context
# as well.

sub check_caller {
    my ($self, $canidate) = @_;
    my $canidate_class = blessed($canidate) || $canidate;
    if ($self->associated_with ne $canidate_class) {
        $self->{caller_error} = "submethod called from the wrong class got: ($canidate_class) expected: (" . $self->associated_with . ")";
        return 0;
    }
    return 1;
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
