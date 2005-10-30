
package Perl6::Core::Num;

use Perl6::Core::Type::Comparable;
use Perl6::Core::Bit;
use Perl6::Core::Str;

package num;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'looks_like_number', 'blessed';

use base 'type::comparable';

sub new {
    my ($class, $value) = @_;
    $value = 0 unless defined $value; # default to 0
    (looks_like_number($value)) 
        || confess "Bad numeric value ($value)";
    bless \$value => $class;
}

# conversion to native value
sub to_native { ${$_[0]} }

# keep this here for a consistent interface
sub to_num { shift }

# conversion to other native types
sub to_bit { (shift)->to_native() ? bit->new(1) : bit->new(0) }
sub to_str { 
    # eventually must deal with Inf and NaN
    return str->new((shift)->to_native() . '');
}

# implement comparitor method

sub _compare {
    my ($left, $right) = @_;
    return $left->to_native <=> $right->to_num->to_native;
}

# methods ...

sub add {
    my ($left, $right) = @_;
    (blessed($right) && $right->isa('type'))
        || confess "The right hand side must be a core type";    
    return num->new($left->to_native + $right->to_num->to_native());
}

sub subtract {
    my ($left, $right) = @_;
    (blessed($right) && $right->isa('type'))
        || confess "The right hand side must be a core type";    
    return num->new($left->to_native - $right->to_num->to_native());
}

sub multiply {
    my ($left, $right) = @_;
    (blessed($right) && $right->isa('type'))
        || confess "The right hand side must be a core type";    
    return num->new($left->to_native * $right->to_num->to_native());
}

sub divide {
    my ($left, $right) = @_;
    (blessed($right) && $right->isa('type'))
        || confess "The right hand side must be a core type";    
    ($right->to_num->to_native != 0)
        || confess "Division by zero error";
    return num->new(int($left->to_native / $right->to_num->to_native()));
}

sub increment { num->new((shift)->to_native + 1) }
sub decrement { num->new((shift)->to_native - 1) }

1;

__END__

=pod

=head1 NAME

num - the core num type

=cut