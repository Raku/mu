package VAST::backslash;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub as_constant_string {
    my $m = shift;
    my $text = $m->{TEXT} // $m->{text}{TEXT};
    if ($text =~ /^\w$/ and $m->{text}) {
        '\\' . $text;
    } elsif ($text =~ /^[n]$/) {
        "\n";
    } elsif ($text =~ /^\W$/) {
        $text;
    } else {
        XXX;
    }
}

1;
