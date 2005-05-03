#!/usr/bin/pugs

use v6;
use Test;

plan 2;

ok( !defined eval_yaml(undef) );
ok( eval_yaml('test') eq 'test' );
