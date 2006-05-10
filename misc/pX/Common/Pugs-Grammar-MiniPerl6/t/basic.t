#!/usr/bin/perl
use lib '../Pugs-Compiler-Rule/lib';
use Test::More tests => 5;
use Pugs::Grammar::MiniPerl6;

sub is_production_match {
    my ($input, $output, @arg) = @_;
    @_ = (Pugs::Grammar::MiniPerl6->ProductionRule($input)->(),
	  $output, @arg);
    print "$_[0]\n";
    goto \&is;
}

is_production_match("return \$<a> + t + 3",
		    'return $ (a + (t + 3))', 'multiple add' );


is_production_match("return App ( 2 )",
		    'return $ (App 2)', 'function application');

is_production_match(q#
          return App(
            Var( doYada( $<sym> ) ),
            Nothing )
#, 'return $ (App (Var (doYada sym)) Nothing)', 'nested function');

is_production_match('return App($1 ~ "backslash: \\"\\\\\\"" ~ $<sym>)',
		    'return $ (App (capture_1 ++ ("backslash: \\"\\\\\\"" ++ sym)))',
		    'function application + string concate + escape char');

is_production_match('return [ "1" ~ $0, "2" ~ $<sym> ]',
		    'return $ [("1" ++ capture_0), ("2" ++ sym)]',
		    'list constructor');

