
package Perl6::Attribute;

use strict;
use warnings;

use constant PUBLIC  => 'public';
use constant PRIVATE => 'private';

sub new {
    my ($class, $associated_with, $label, $type) = @_;
    my $visibility = PUBLIC;
    $visibility = PRIVATE if $label =~ /^.\:/;
    my ($accessor_name) = ($label =~ /^..(.*)$/);
    bless {
        associated_with => $associated_with,
        accessor_name   => $accessor_name,
        visibility      => $visibility,
        type            => $type,
        label           => $label,
    }, ref($class) || $class;
}

sub type { (shift)->{type} }

# this is for type checking (sort of)
sub is_array { (shift)->{label} =~ /^\@/ }
sub is_hash  { (shift)->{label} =~ /^\%/ }

sub associated_with { (shift)->{associated_with} }
sub accessor_name   { (shift)->{accessor_name}   }

sub is_private { (shift)->{visibility} eq 'private' }
sub is_public  { (shift)->{visibility} eq 'public'  }

1;

__END__

=pod

=head1 NAME

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new>

=item B<type>

=item B<is_array>

=item B<associated_with>

=item B<accessor_name>

=item B<is_private>

=item B<is_public>

=back

=head1 AUTHOR

Stevan Little

=cut
