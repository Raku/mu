#!/usr/bin/perl
package main;
use Pugs::Grammar::MiniPerl6;
use Data::Dumper;

my $match1 = Pugs::Grammar::MiniPerl6->ProductionRule("return \$<a> + t + 3");

print $match1->();  # return $ (a + (t + 3))

my $match2 = Pugs::Grammar::MiniPerl6->ProductionRule("return App ( 2 )");

print $match2->();  # return $ (App 2)

my $match3 = Pugs::Grammar::MiniPerl6->ProductionRule(q#
          return App(
            Var( doYada( $<sym> ) ),
            Nothing )
#);

print $match3->();  # return $ (App (Var (doYada sym)) Nothing)

my $match4 = Pugs::Grammar::MiniPerl6->ProductionRule("return App(\$1 ~ \$<sym>)");

print $match4->();  # return $ (App (capture_1 ++ sym))

