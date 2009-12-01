package VAST::integer;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    #XXX non-base 10
    integer($m->{TEXT});
}

1;
