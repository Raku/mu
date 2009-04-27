use strict;
use warnings;

use Test::More;
use Test::NoWarnings;

plan tests => 5 + 1;

# L<Text::SmartLinks/NAME>

use_ok('Text::SmartLinks');

# L<Text::SmartLinks/new>

my $sl = Text::SmartLinks->new;
isa_ok($sl, 'Text::SmartLinks');

# L<Text::SmartLinks/get_javascript>

my $js = $sl->get_javascript();
cmp_ok(length($js), '>', 100, 'js has some bytes');

# L<Text::SmartLinks/link_count>

is $sl->link_count, 0, 'link_count 0';
$sl->link_count_inc;
is $sl->link_count, 1, 'link_count 1';
