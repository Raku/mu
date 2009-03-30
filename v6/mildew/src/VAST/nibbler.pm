package VAST::nibbler;
use utf8;
use strict;
use warnings;
use Scalar::Util 'blessed';
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    return string $m->as_constant_string;
}

sub as_constant_string {
    my $m = shift;
    my $str = '';

    return $m->{TEXT};
}

1;
