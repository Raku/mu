
package Perl6::Attribute;

use strict;
use warnings;

use Scalar::Util 'blessed';

use constant PUBLIC  => 'public';
use constant PRIVATE => 'private';

sub new {
    my ($class, $associated_with, $label, $type) = @_;
    my $visibility = PUBLIC;
    $visibility = PRIVATE if $label =~ /^.\:/;
    my ($accessor_name) = ($label =~ /^..(.*)$/);
    my $attr = bless {
        associated_with => $associated_with,
        accessor_name   => $accessor_name,
        visibility      => $visibility,
        type            => $type,
        label           => $label,
    }, $class;
    return $attr;
}

sub type  { (shift)->{type}  }
sub label { (shift)->{label} }

# this is for type checking (sort of)
sub is_array { (shift)->{label} =~ /^\@/ }
sub is_hash  { (shift)->{label} =~ /^\%/ }

sub associated_with { (shift)->{associated_with} }
sub accessor_name   { (shift)->{accessor_name}   }

sub is_private { (shift)->{visibility} eq 'private' }
sub is_public  { (shift)->{visibility} eq 'public'  }

sub instantiate_container {
    my ($self) = @_;
    ($self->is_array ? [] : ($self->is_hash ? {} : undef));
}

1;

__END__

=pod

=head1 NAME

Perl6::Attribute - Base class for Attribute in the Perl6 Meta Model

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

Stevan Little stevan@iinteractive.com

=cut
