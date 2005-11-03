
package Perl6::Core::Bit;

use Perl6::Core::Type;
use Perl6::Core::Str;
use Perl6::Core::Num;

package bit;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed', 'looks_like_number';

use base 'type';

our $TRUE  = bless \(my $t = 1) => __PACKAGE__;
our $FALSE = bless \(my $f = 0) => __PACKAGE__;

sub new {
    my (undef, $value) = @_;
    (defined $value            && 
     looks_like_number($value) && 
     ($value == 1 || $value == 0))
        || confess "Bad bit value ($value)";    
    return $TRUE if $value;
    return $FALSE;
}

# conversion to native value
sub to_native { ${$_[0]} }

# keep this here for a consistent interface
sub to_bit { shift }

# conversion to other native types
sub to_str { (shift)->to_native ? str->new('1') : str->new('')  }
sub to_num { (shift)->to_native == 0 ? num->new(0) : num->new(1) }

# methods 

sub and : method {
    my ($self, $block) = @_;
    (blessed($block) && $block->isa('block'))
        || confess "AND takes a block";
    if ($self == $TRUE) {
        $block->do();
        return $TRUE;
    }
    else {
        return $FALSE;
    }
}

sub or : method {
    my ($self, $block) = @_;
    (blessed($block) && $block->isa('block'))
        || confess "OR takes a block";    
    if ($self == $FALSE) {
        $block->do();
        return $TRUE;
    }
    else {
        return $FALSE;
    }        
}

1;

__END__

=pod

=head1 NAME

bit - the core bit type

=head1 METHODS

=over 4

=item B<new (*native*) returns bit>

=item B<to_native () returns *native*>

=item B<to_bit () returns bit>

=item B<to_num () returns num>

=item B<to_str () returns str>

=back

=cut