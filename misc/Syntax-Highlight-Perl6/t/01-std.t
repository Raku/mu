use strict;
use warnings;
use Test::More tests => 4;

use STD;

#check that we have get back a parser when we have an empty string
my $r = STD->parse('');
ok(defined $r, 'STD->parse() returned something on a empty string');
isa_ok( $r, 'STD', 'STD->parse() return type');

#check redeclared Bar STD bug
my $s = STD->parse('module Foo::Bar { }; module Moo::Bar;');
ok(defined $s, 'STD->parse() worked');
isa_ok( $s, 'STD', 'STD->parse() return type');
