package VAST::longname;
use utf8;
use strict;
use warnings;
use AST::Helpers;


sub canonical {
    my $m = shift;
    my $single_variant = '';
    if ($m->{colonpair}[1]) {
       if ($m->{colonpair}[1]{signature}) {
           # TODO handle whitespace sensibly
           $single_variant = ':(' . $m->{colonpair}[1]{signature}->{MATCH}->Str . ')';
       } else {
           XXX;
       } 
    }
    my $name = $m->{name}{identifier}{TEXT};
    my $v = $m->{colonpair}[0]{v};
    if ($v) {
        $name . ':' . $v->{circumfix}{nibble}->Str . $single_variant;
    } else {
        $name;
    }
}
1;
