
package Perl6::Method;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

our @CURRENT_CLASS_STACK;
our @CURRENT_INVOCANT_STACK;

sub new {
    my ($class, $associated_with, $code) = @_;
    (defined $associated_with && defined $code) 
        || confess "Insufficient Arguments : You must provide a class this is associated with and code";    
    (ref($code) eq 'CODE') 
        || confess "Incorrect Object Type : The code arguments must be a CODE reference";
    bless {
        associated_with => $associated_with,
        code            => $code,
    }, $class;
}

sub associated_with { (shift)->{associated_with} }
sub call { 
    my ($self, @args) = @_;  
    push @CURRENT_CLASS_STACK => $self->{associated_with};
    push @CURRENT_INVOCANT_STACK => $args[0] if blessed($args[0]);    
    my @rval = $self->{code}->(@args); 
    pop @CURRENT_INVOCANT_STACK if blessed($args[0]);
    pop @CURRENT_CLASS_STACK;
    return wantarray ? @rval : $rval[0];
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
