#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

=cut

my $IR = Baboon.new but {
    .butt = "red"; # :todo<feature> # but texture("shiney");
};

is($IR.butt, "red", "basic anatomy");

# vim:set ft=perl6
