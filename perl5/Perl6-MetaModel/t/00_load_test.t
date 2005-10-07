#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;
use Test::Exception;

# this loads all the libs in order ...

lives_ok {
    require Perl6::MetaModel::Genesis;
} '... we can load genesis ok';
