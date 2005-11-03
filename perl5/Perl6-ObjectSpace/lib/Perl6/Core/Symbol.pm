
package Perl6::Core::Symbol;

use Perl6::Core::Type::Comparable;
use Perl6::Core::Num;
use Perl6::Core::Bit;
use Perl6::Core::Str;

package symbol;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type::comparable';

sub new {
    my ($class, $name, $type) = @_;
    (defined $name) 
        || confess "You must provide a name for your symbol";
    bless { 
        name => $name,
        type => $type || 'type',
    } => $class;
}

# conversion to native value
sub to_native { shift }

# keep this here for a consistent interface
sub to_str { str->new((shift)->{name}) }

# conversion to other native types
sub to_bit { bit->new(1)           }
sub to_num { num->new((shift) + 0) }

# methods

sub name { str->new((shift)->{name}) }
sub type { str->new((shift)->{type}) }

sub equal_to {
    my ($left, $right) = @_;
    return bit->new(0) 
        unless blessed($right) && $right->isa('symbol');
    return bit->new(1) 
        if $left->name eq $right->name && 
           $left->type eq $right->type;
    return bit->new(0);
}

sub not_equal_to {
    my ($left, $right) = @_;
    ($left->equal_to($right) == bit->new(1)) ? bit->new(0) : bit->new(1);
}

sub _compare {
    confess "Can only compare the equality of a symbol";
}

1;

__END__

=pod

=head1 NAME

symbol - the core symbol type

=head1 METHODS

=over 4

=item B<new (*native*) returns str>

=item B<to_native () returns *native*>

=item B<to_bit () returns bit>

=item B<to_num () returns num>

=item B<to_str () returns str>

=back

=cut