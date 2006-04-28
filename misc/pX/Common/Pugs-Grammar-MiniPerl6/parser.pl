#!/usr/bin/perl

use Pugs::Grammar::MiniPerl6;
use Data::Dumper;

my $match = Pugs::Grammar::MiniPerl6->Return("return 3");

print $match->();

