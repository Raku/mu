package VAST::escape;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub as_constant_string {
    my $m = shift;
    $m->{item}->as_constant_string;
}

1;
