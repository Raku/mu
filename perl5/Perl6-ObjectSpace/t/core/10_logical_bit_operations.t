#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::Bit');
use_ok('Perl6::Core::Block');

my $e = closure::env->new();

bit->new(1)
->and(
  block->new($e, sub { pass('... bit::and worked correctly') })
)->or(
  block->new($e, sub { fail('... bit::and did not work correctly') })    
);

bit->new(0)
->and(
  block->new($e, sub { fail('... bit::or did not work correctly') })    
)->or(
  block->new($e, sub { pass('... bit::or worked correctly') })
);

