package VAST::Base;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub as_constant_string {
    my $m = shift;
    use YAML::XS;
    die Dump($m)." can't be represented as a constant string\n";
}

1;

