use strict;
use warnings;

use Test::More;
use Test::NoWarnings;

plan tests => 5 + 1;

# L<Smart::Links/NAME>

use_ok('Smart::Links');

# L<Smart::Links/new>

my $sl = Smart::Links->new;
isa_ok($sl, 'Smart::Links');

# L<Smart::Links/get_javascript>

my $js = $sl->get_javascript();
cmp_ok(length($js), '>', 100, 'js has some bytes');

# L<Smart::Links/link_count>

is $sl->link_count, 0, 'link_count 0';
$sl->link_count_inc;
is $sl->link_count, 1, 'link_count 1';