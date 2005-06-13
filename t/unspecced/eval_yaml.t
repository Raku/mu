#!/usr/bin/pugs

use v6;
use Test;

plan 2;

ok( !defined eval(undef, :lang<yaml>) );
ok( eval('test', :lang<yaml>) eq 'test' );
