
package Perl6::Core::Ref;

use Perl6::Core::Type;

package ref;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my ($class, $value) = @_;
    (blessed($value) && $value->isa('type'))
        || confess "You must provide a value to reference";
    bless \$value => $class;
}

sub to_native { shift }

# conversion to other native types
sub to_num {}
sub to_bit {}
sub to_str {}


1;

__END__

=pod

=head1 NAME

ref - the core ref type

=cut