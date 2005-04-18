#!/usr/bin/pugs

use v6;
require Test;

plan 10;

ok("a" eq "a");
ok(!("a" eq "ab"));

ok(!("a" ne "a"));
ok("a" ne "ab");

ok("\0" eq "\0");

my $foo;
ok ($foo eq "");
ok (!($foo eq "f"));

my @foo;
ok (@foo[0] eq "");
ok (@foo[0] ne "f");

@foo = eval '';
ok (@foo[0] ne "f");
