
package Perl6::Core::Type::Comparable;

use Perl6::Core::Type::Equivalence;

package type::comparable;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

use base 'type::equivalence';

# need to load this here in 
# order for things to init 
# in the correct order
use Perl6::Core::Bit;

# comparisons

# implement this and get the rest for free
# NOTE: 
# compare returns a native boolean though,
# so it should be considered a protected 
# method, which is used to implement the 
# other methods below
sub _compare { confess 'You must override _compare in ' . (shift) } 

sub equal_to     { $_[0]->_compare($_[1]) == 0  ? bit->new(1) : bit->new(0) }

sub greater_than { $_[0]->_compare($_[1]) == 1  ? bit->new(1) : bit->new(0) }
sub less_than    { $_[0]->_compare($_[1]) == -1 ? bit->new(1) : bit->new(0) }

sub greater_than_or_equal_to { $_[0]->_compare($_[1]) >= 0  ? bit->new(1) : bit->new(0) }
sub less_than_or_equal_to    { $_[0]->_compare($_[1]) <= 0  ? bit->new(1) : bit->new(0) }

1;

__END__

=pod

=head1 NAME

type::comparable - a comparable type

=cut