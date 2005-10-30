#!/usr/bin/pugs

use Test;
use v6;

plan 4;

=head1 DESCRIPTION

This test tests the C<pick> builtin.

Closest I could find to documentation:
L<"http://groups.google.com/group/perl.perl6.language/tree/browse_frm/thread/24e369fba3ed626e/4e893cad1016ed94?rnum=1&_done=%2Fgroup%2Fperl.perl6.language%2Fbrowse_frm%2Fthread%2F24e369fba3ed626e%2F6e6a2aad1dcc879d%3F#doc_2ed48e2376511fe3"> 

=cut

my @array = <a b c d>;
ok ?(@array.pick eq any <a b c d>), "pick works on arrays";

my %hash = (a => 1);
is %hash.pick.key,   "a", "pick works on hashes (1)";
is %hash.pick.value, "1", "pick works on hashes (2)";

my $junc = (1|2|3);
ok ?(1|2|3 == $junc.pick), "pick works on junctions";
