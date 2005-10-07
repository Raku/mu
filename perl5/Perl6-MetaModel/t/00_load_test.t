#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;
use Test::Exception;

# this loads all the libs in order ...

lives_ok {
    require 'lib/genesis.pl';
} '... we can load genesis ok';
