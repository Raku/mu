
package Perl6::Core::Type::Equivalence;

use Perl6::Core::Type;

package type::equivalence;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

use base 'type';

# equality

sub equal_to     { confess 'You must override equal_to in ' . (shift) }
sub not_equal_to { $_[0]->equal_to($_[1]) == bit->new(0)  ? bit->new(1) : bit->new(0) }

1;

__END__

=pod

=head1 NAME

type::comparable - a comparable type

=cut