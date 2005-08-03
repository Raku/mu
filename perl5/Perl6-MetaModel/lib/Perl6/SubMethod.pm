
package Perl6::SubMethod;

use strict;
use warnings;

use Scalar::Util 'blessed';

use base 'Perl6::Method';

sub new {
    my ($class, $associated_with, $code) = @_;
    my $self = $class->SUPER::new($associated_with, $code);
    my $old = $self->{code};
    $self->{old_code} = $old;
    $self->{code} = sub {
        my ($self, @args) = @_;  
        return ::next_METHOD() if $args[0]->{class} ne $self->associated_with; 
        $old->($self, @args); 
    };
    return $self;     
}

# XXX - this just bypasses the local do()
sub force_call { 
    my ($self, @args) = @_;  
    $self->{old_code}->($self, @args);     
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
