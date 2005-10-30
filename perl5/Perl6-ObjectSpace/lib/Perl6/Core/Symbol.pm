
package Perl6::Core::Symbol;

use Perl6::Core::Type;

package symbol;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type';

sub new {
    my ($class, $value) = @_;
    (blessed($value) && $value->isa('type'))
        || confess "You must provide a value to symbolize";
    bless \$value => $class;
}

1;

__END__

=pod

=head1 NAME

symbol - the core symbol type

=cut