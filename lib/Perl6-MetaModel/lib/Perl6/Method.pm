
package Perl6::Method;

use strict;
use warnings;

sub new {
    my ($class, $associated_with, $code) = @_;
    bless {
        associated_with => $associated_with,
        code            => $code,
    }, $class;
}

sub associated_with { (shift)->{associated_with} }
sub call { 
    my ($self, @args) = @_;
    $self->{code}->(@args); 
}

1;