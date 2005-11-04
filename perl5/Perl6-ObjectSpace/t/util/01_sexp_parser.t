#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Util::SExpParser');

# string->new("hello world")->say
my $source = '((string new ("hello world")) say)';

my $output = Perl6::Util::SExpParser->parse($source);

#use Data::Dumper;
#diag Dumper $output;

is_deeply(
    $output,
    [['string', 'new', ['"', 'hello', 'world', '"']], 'say'],
    '... got the right structure');