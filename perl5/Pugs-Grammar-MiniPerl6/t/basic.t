#!/usr/bin/perl
use lib '../Pugs-Compiler-Rule/lib';
use Test::More tests => 8;
use Pugs::Grammar::MiniPerl6;
use Pugs::Runtime::Match::Ratchet;

sub is_production_match {
    my ($input, $output, @arg) = @_;
    my $got = Pugs::Grammar::MiniPerl6->ProductionRule($input);
    @_ = ("$got", $output, @arg);
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

is_production_match('my $a = 1 + 2; return $a - 3;',
		    "let a = (1 + 2) in\nreturn \$ (a - 3)",
		    'variable declaration');

is_production_match(q#
   my $yada = doYada( $<sym> );
   my $err = Val( VStr( $<sym> ~ " - not yet implemented") );
   return App(
    Var( $yada ),
    Nothing,
    [ $err, $0 ]
   )
#, q#let yada = (doYada sym) in
let err = (Val (VStr (sym ++ " - not yet implemented"))) in
return $ (App (Var yada) Nothing [err, capture_0])#, 'full yada example');

is_production_match(' return; ', 'return ()', 'return ()');
