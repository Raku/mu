use strict;
use warnings;

use Test::More;
use Test::NoWarnings;

plan tests => 3 + 1;

use_ok('Smart::Links');

my $sl = Smart::Links->new;
isa_ok($sl, 'Smart::Links');

my $js = $sl->get_javascript();
cmp_ok(length($js), '>', 100, 'js has some bytes');
