#!/usr/bin/perl
package main;
use Pugs::Grammar::MiniPerl6;
use Data::Dumper;

my $match = Pugs::Grammar::MiniPerl6->ProductionRule("return \$<a> + t + 3");

print $match->();  # return $ (a + (t + 3))
print $match->from;
print $match->to;

