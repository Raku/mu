#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MetaModel::Bootstrap');

is(::send($::Class, str->new('has_method'), list->new($::Class, str->new('has_method'))),
   $bit::TRUE,
   '... we have the methods "has_method" in our class');

is(::send($::Class, str->new('has_method'), list->new($::Class, str->new('add_method'))),
   $bit::TRUE,
   '... we have the methods "add_method" in our class');