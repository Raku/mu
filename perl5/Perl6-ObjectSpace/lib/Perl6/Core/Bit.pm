
package Perl6::Core::Bit;

use Perl6::Core::Type;
use Perl6::Core::Str;
use Perl6::Core::Num;

package bit;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'looks_like_number';

use base 'type';

sub new {
    my ($class, $value) = @_;
    $value = 0 unless defined $value; # default to false
    (looks_like_number($value) && ($value == 1 || $value == 0))
        || confess "Bad bit value ($value)";
    bless \$value => $class;
}

# conversion to native value
sub to_native { ${$_[0]} }

# keep this here for a consistent interface
sub to_bit { shift }

# conversion to other native types
sub to_str { (shift)->to_native ? str->new('1') : str->new('0')  }
sub to_num { (shift)->to_native == 0 ? num->new(0) : num->new(1) }

1;

__END__

=pod

=head1 NAME

bit - the core bit type

=cut