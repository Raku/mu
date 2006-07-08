use v6-alpha;
use Test;

plan 1;

use Text::Glob;

my $regex = Text::Glob.new.glob_to_regex('*.pm');
ok 'Glob.pm' ~~ $regex, 'glob_to_regex works';
