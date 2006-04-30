#!/usr/bin/perl
package main;
use Pugs::Grammar::MiniPerl6;
use Data::Dumper;

my $match = Pugs::Grammar::MiniPerl6->ProductionRule("return \$<a> + 3");

print $match->();  # return $ a + 3

