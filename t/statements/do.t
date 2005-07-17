#!/usr/bin/pugs

use v6;
use Test;

plan 2;

my $ret = do given 3 {
    when 3 { 1 }
};
is($ret, 1, 'do STMT works');

my $ret = do { given 3 {
    when 3 { 1 }
} };
is($ret, 1, 'do { STMT } works');
