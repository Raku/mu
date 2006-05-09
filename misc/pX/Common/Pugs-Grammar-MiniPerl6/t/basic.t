#!/usr/bin/perl
package main;
use lib '../Pugs-Compiler-Rule/lib';
use Pugs::Grammar::MiniPerl6;
use Test::More tests => 4;

sub is_production_match {
    my ($input, $output, @arg) = @_;
    @_ = (Pugs::Grammar::MiniPerl6->ProductionRule($input)->(),
	  $output, @arg);
    goto \&is;
}

is_production_match("return \$<a> + t + 3",
		    '    return $ (a + (t + 3))' );


is_production_match("return App ( 2 )",
		    '    return $ (App 2)');

is_production_match(q#
          return App(
            Var( doYada( $<sym> ) ),
            Nothing )
#, '    return $ (App (Var (doYada sym)) Nothing)');

is_production_match('return App($1 ~ "backslash: \\"\\\\\\"" ~ $<sym>)',
		    '    return $ (App (capture_1 ++ ("backslash: \"\\\"" ++ sym)))');

is_production_match('return [ "1" ~ $0, "2" ~ $<sym> ]',
		    '    return $ [("1" ++ capture_0), ("2" ++ sym)]');

