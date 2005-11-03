
package Perl6::MM::Attribute;

use Perl6::Core::Symbol;

package attribute;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'symbol';

sub new {
    my ($class, $name, $type) = @_;
    (defined $name && $name =~ /^[\$\@\%\&][\.\:][a-zA-Z0-9_]+$/)
        || confess "bad attribute name ($name)";
    $class->SUPER::new($name, $type);
}

sub instantiate_container {
    my $self = shift;
    return $self->{type}->new();
}

1;

__END__

=pod

=head1 NAME

attribute - the core attribute type

=cut
