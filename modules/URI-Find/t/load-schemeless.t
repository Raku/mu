#!/usr/bin/perl6

use Test;
BEGIN { plan 1 }

use URI::Find::Schemeless;
ok URI::Find::Schemeless.new(callback => ->{});
