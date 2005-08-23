#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 2;
use Test::Exception;

do 'lib/pneuma.pl';

is($::Object->name, 'Object', '... $::Object->name() is Object');
is($::Object->version, '0.0.0', '... $::Object->version() is 0.0.0');