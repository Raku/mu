#!/usr/bin/pugs

use v6;
require Test;

plan 4;

# Some Chinese Characters
ok(eval 'my $一;', "chinese name declaration ok");

my $二 = 2;
is($二,2,"..... assignment ok");

# Tibeten Characters
ok(eval 'my $༡;', "tibeten name declaration ok");
my $༢ = 2;
is($༢,1, "..... assignment ok");
