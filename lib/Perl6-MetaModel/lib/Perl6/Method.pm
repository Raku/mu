
package Perl6::Method;

use strict;
use warnings;

sub new {
    my ($class, $associated_with, $code) = @_;
    bless {
        associated_with => $associated_with,
        code            => $code,
    }, $class;
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

Perl6::Method - Base class for Methods in the Perl6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new>

=item B<code>

=item B<associated_with>

=back

=head1 AUTHOR

Stevan Little stevan@iinteractive.com

=cut
