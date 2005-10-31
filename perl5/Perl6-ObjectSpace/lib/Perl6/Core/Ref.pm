
package Perl6::Core::Ref;

use Perl6::Core::Type;
use Perl6::Core::Str;
use Perl6::Core::Num;
use Perl6::Core::Bit;

package reference;

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
sub to_num { num->new((shift)->fetch + 0)  }
sub to_str { str->new((shift)->fetch . '') }
sub to_bit { (shift)->fetch == $nil::NIL ? bit->new(1) : bit->new(0) }

# methods

sub fetch { ${$_[0]} }
sub store { 
    my ($self, $value) = @_;
    (blessed($value) && $value->isa('type'))
        || confess "You must provide a value to reference";    
    ${$self} = $value;
    return nil->new();
}

1;

__END__

=pod

=head1 NAME

reference - the core reference type

=head1 METHODS

=over 4

=item B<new (~type) returns reference>

=item B<to_native () returns *native*>

=item B<to_bit () returns bit>

=item B<to_num () returns num>

=item B<to_str () returns str>

=item B<fetch () returns ~type>

=item B<store (~type) returns nil>

=back

=cut