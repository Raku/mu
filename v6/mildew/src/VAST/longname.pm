package VAST::longname;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub canonical {
    my $m = shift;
    my $name = $m->{name}{identifier}{TEXT};
    my $v = $m->{colonpair}[0]{v}{nibble}{nibbles}[0];
    if ($v) {
        $name . ':' . $v;
    } else {
        $name;
    }
}

1;
