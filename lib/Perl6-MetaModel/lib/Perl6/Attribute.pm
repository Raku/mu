
package Perl6::Attribute;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use Perl6::Container::Scalar;

use constant PUBLIC  => 'public';
use constant PRIVATE => 'private';

sub new {
    my ($class, $associated_with, $label, $props) = @_;
    (defined $associated_with && defined $label) 
        || confess "Insufficient Arguments : You must provide a class this is associated with and a label";
    my $visibility = PUBLIC;
    $visibility = PRIVATE if $label =~ /^.\:/;
    my ($accessor_name) = ($label =~ /^..(.*)$/);
    (defined $accessor_name) 
        || confess "Bad label : could not extract accessor name from label ($label)";
    if (defined $props) {
        $props->{access} = 'ro'  unless exists $props->{access};
        $props->{type}   = undef unless exists $props->{type};        
    }
    else {
        $props = { access => 'ro', type => undef };
    }
    my $attr = bless {
        associated_with => $associated_with,
        accessor_name   => $accessor_name,
        visibility      => $visibility,
        properties      => $props,
        label           => $label,
    }, $class;
    return $attr;
}

sub is_ro { (shift)->{properties}->{access} eq 'ro' }
sub is_rw { (shift)->{properties}->{access} eq 'rw' }
sub type  { (shift)->{properties}->{type}           }

sub label { (shift)->{label} }

# this is for type checking (sort of)
sub is_array { (shift)->{label} =~ /^\@/ }
sub is_hash  { (shift)->{label} =~ /^\%/ }

sub associated_with { (shift)->{associated_with} }
sub accessor_name   { (shift)->{accessor_name}   }

sub is_private { (shift)->{visibility} eq PRIVATE }
sub is_public  { (shift)->{visibility} eq PUBLIC  }

sub instantiate_container {
    my ($self) = @_;
    return \([]) if $self->is_array;
    return \({}) if $self->is_hash; 
    return \(my $scalar);
}

1;

__END__

=pod

=head1 NAME

Perl6::Attribute - Base class for Attribute in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new ($associated_with, $label, ?$type)>

=item B<type>

=item B<label>

=item B<is_array>

=item B<associated_with>

=item B<accessor_name>

=item B<is_private>

=item B<is_public>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
