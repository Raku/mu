
package Perl6::Core::Type;

package type;

use strict;
use warnings;

## Define the basic interface for all types

# conversion to native
sub to_native;

# conversion to other native types
sub to_num;
sub to_bit;
sub to_str;

1;

__END__

=pod

=head1 NAME 

type - the base native type 'type'

=cut
