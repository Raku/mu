#!/usr/bin/pugs

use v6;
use Test;

plan 2;

if $?PUGS_BACKEND eq "BACKEND_JAVASCRIPT" {
  skip_rest "YAML support not available in PIL2JS";
  exit;
}

ok( !defined eval(undef, :lang<yaml>) );
ok( eval('test', :lang<yaml>) eq 'test' );
