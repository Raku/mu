#!/usr/bin/pugs

use v6;
use Test;

my $dollar_underscore;
lives_ok {
    $_ = 42;
    {
	$dollar_underscore = $_;
    }
}, 'bare blocks containing $_ work correctly (1)';

is $dollar_underscore, 42, 'bare blocks containing $_ work correctly (2)';
