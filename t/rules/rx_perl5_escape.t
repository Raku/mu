#!/usr/bin/pugs

use v6;
use Test;

BEGIN { plan 1 }

my $str = "http://foo.bar/";
fail "XXX parsefail", :todo<bug>;
# ok(($str ~~ rx:perl5/http:\/\//), "test the regular expression escape");

