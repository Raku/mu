
package Perl6::Core::Nil;

use Perl6::Core::Type;

package nil;

use strict;
use warnings;

use base 'type';

sub new { bless \(my $var) => (shift) }

# conversion to native
sub to_native { undef }

# conversion to other native types
sub to_num { num->new(0)  }
sub to_bit { bit->new(0)  }
sub to_str { str->new('') }

1;

__END__

=pod

=head1 NAME 

nil - the base native nil type

=cut
