
package Perl6::SubMethod;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Perl6::MetaModel '-no_import';

use base 'Perl6::Method';

sub call { 
    my ($self, @args) = @_;  
    return Perl6::MetaModel::next_METHOD() 
        if blessed(Perl6::MetaModel::SELF()) ne Perl6::MetaModel::CLASS(); 
    $self->{code}->(@args); 
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
