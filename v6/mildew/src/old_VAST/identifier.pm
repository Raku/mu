package VAST::identifier;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    return string $m->{TEXT};
}

1;
