#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

=cut

my $IR = Baboon.new but {
    # no copy -- $_ is the frsh Baboon.new
    .butt = "red"; # :todo<feature> # but texture("shiney");
    # here is an implicit ($_;)  to get the value back to $IR.
};

# my $IR = do given Baboon.new {
#     .butt = "ref";
#     $_;
# };

is($IR.butt, "red", "basic anatomy");

# vim:set ft=perl6
