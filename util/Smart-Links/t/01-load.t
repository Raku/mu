use strict;
use warnings;

use Test::More;
use Test::NoWarnings;

plan tests => 2 + 1;
#diag explain \%ENV;

use_ok('Smart::Links');
my $js = get_javascript();
cmp_ok(length($js), '>', 100, 'js has some bytes');
