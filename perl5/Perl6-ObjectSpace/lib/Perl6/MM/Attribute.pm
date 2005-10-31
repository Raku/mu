
package Perl6::MM::Attribute;

use Perl6::Core::Str;

package attribute;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'str';

sub new {
    my ($class, $name) = @_;
    (defined $name && $name =~ /^[\$\@\%\&][\.\:][a-zA-Z0-9_]+$/)
        || confess "bad attribute name ($name)";
    $class->SUPER::new($name);
}

1;

__END__

=pod

=head1 NAME

attribute - the core attribute type

=cut
