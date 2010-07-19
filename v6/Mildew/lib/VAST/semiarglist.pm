package VAST::semiarglist;
use utf8;
use strict;
use warnings;
use Mildew::AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    $m->{arglist}[0]->emit_m0ld;
}

1;
