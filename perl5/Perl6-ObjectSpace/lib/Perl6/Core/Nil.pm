
package Perl6::Core::Nil;

use Perl6::Core::Type;

package nil;

use strict;
use warnings;

use base 'type';

our $NIL = bless \(my $var) => __PACKAGE__;
sub new { $NIL }

# conversion to native
sub to_native { undef }

# conversion to other native types
sub to_num { num->new(0)  }
sub to_bit { bit->new(0)  }
sub to_str { str->new('') }

sub is_nil { bit->new(1) }

1;

__END__

=pod

=head1 NAME 

nil - the base native nil type

=head1 METHODS

=over 4

=item B<new (*native*) returns nil>

=item B<to_native () returns *native*>

=item B<to_bit () returns bit>

=item B<to_num () returns num>

=item B<to_str () returns str>

=back

=cut
