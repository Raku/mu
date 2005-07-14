
package Perl6::Method;

use strict;
use warnings;

use Carp 'confess';

sub new {
    my ($class, $associated_with, $code) = @_;
    (defined $associated_with && defined $code) 
        || confess "Insufficient Arguments : You must provide a class this is associated with and code";    
    (ref($code) eq 'CODE') 
        || confess "Incorrect Object Type : The code arguments must be a CODE reference";
    bless {
        associated_with => $associated_with,
        code            => $code,
        caller_error    => undef
    }, $class;
}

# override this to check methods
sub check_caller { 1 }

# it disappears as soon as you look at it :)
sub caller_error {
    my ($self) = @_;
    return unless $self->{caller_error};
    my $err = $self->{caller_error};
    $self->{caller_error} = undef;
    return $err;
}

sub associated_with { (shift)->{associated_with} }
sub call { 
    my ($self, @args) = @_;   
    $self->{code}->(@args); 
}

1;

__END__

=pod

=head1 NAME

Perl6::Method - Base class for Methods in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new ($associated_with, $code)>

=item B<call (@args)>

=item B<associated_with>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
