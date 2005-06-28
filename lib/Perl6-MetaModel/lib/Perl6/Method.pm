
package Perl6::Method;

use strict;
use warnings;

sub new {
    my ($class, $associated_with, $code) = @_;
    bless {
        associated_with => $associated_with,
        code            => $code,
    }, ref($class) || $class;
}

sub associated_with { (shift)->{associated_with} }
sub call { (shift)->{code}->(@_) }

1;