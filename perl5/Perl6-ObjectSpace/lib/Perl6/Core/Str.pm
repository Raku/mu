
package Perl6::Core::Str;

use Perl6::Core::Type::Comparable;
use Perl6::Core::Num;
use Perl6::Core::Bit;

package str;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type::comparable';

sub new {
    my ($class, $value) = @_;
    $value = '' unless defined $value; # default to empty string
    bless \$value => $class;
}

# conversion to native value
sub to_native { ${$_[0]} }

# keep this here for a consistent interface
sub to_str { shift }

# conversion to other native types
sub to_bit { (shift)->to_native() ? bit->new(1) : bit->new(0) }
sub to_num { 
    # eventually must deal with Inf and NaN
    no warnings 'numeric';
    return num->new((shift)->to_native() + 0);
}

# implement comparitor method

sub _compare {
    my ($left, $right) = @_;
    return $left->to_native cmp $right->to_str->to_native;
}

# methods ... 
 
sub concat {
    my ($left, $right) = @_;
    (blessed($right))
        || confess "The right hand side must be a core type";
    return str->new($left->to_native . $right->to_str->to_native);
}

1;

__END__

=pod

=head1 NAME

str - the core str type

=cut