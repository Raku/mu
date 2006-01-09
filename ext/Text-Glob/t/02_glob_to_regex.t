#!/usr/bin/pugs

use v6;
use Test;

plan 1;

use Text::Glob;

my $regex = Text::Glob.new.glob_to_regex('*.pm');
ok 'Glob.pm' ~~ $regex, 'glob_to_regex works';
