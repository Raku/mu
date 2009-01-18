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
    foreach my $nib (@{$m->{nibbles}}) {
        if (blessed $nib) {
            $str .= $nib->as_constant_string;
        } else {
            $str .= $nib;
        }
    }
    return $str;
}

1;
