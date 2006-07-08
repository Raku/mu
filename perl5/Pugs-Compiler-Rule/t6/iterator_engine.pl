# pX/Common/p6rule.t - fglock

use v6;
use Pugs::Runtime::Rule;
#use strict;
#use warnings;

#require 'iterator_engine.pl';

#use Test;  -- errors?
sub ok($val,$comment) { 
  print "not " unless $val;
  print "ok - $comment\n";
}

#use Data::Dumper;
#$Data::Dumper::Indent = 1;
#$Data::Dumper::Pad = '# ';

ok( 1, "compiled" );

my ( $rule, $match );

{
  $rule = 
        ruleop::constant( 'a' ), 
    ;
  $match = $rule( 'a123', undef, {capture=>1,} );
  ok( $match<bool>, "/a/ #1" );
  ok( $match<tail> eq '123', "tail is ok" );
  $match = $rule( 'c123', undef, {capture=>1,} );
  ok( !$match<bool>, "/a/ #2" );
  # XXX - should this work?
  #ok( $match<tail> eq 'c123', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::non_greedy_plus( 
      ruleop::alternation( [
        ruleop::constant( 'a' ), 
        ruleop::constant( 'c' ), 
      ] ),
    );
  $match = $rule( 'a123', undef, {capture=>1,} );
  ok( $match<bool>, "/[a|c]/ #1" );
  ok( $match<tail> eq '123', "tail is ok" );
  $match = $rule( 'c123', undef, {capture=>1,} );
  ok( $match<bool>, "/[a|c]/ #2" );
  ok( $match<tail> eq '123', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::greedy_star( 
      ruleop::constant( 'a' ) 
    );
  # XXX - not portable?
  #ok( ref $rule eq "CODE", "rule 'a*' is a coderef" );
  $match = $rule( 'aa' );
  # print Dumper( $match );
  ok( $match<bool>, "/a*/" );
  #print Dumper( $match );
  $match = $rule( '' );
  ok( $match<bool>, "matches 0 occurrences" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::greedy_plus( 
      ruleop::constant( 'a' ) 
    );
  $match = $rule( 'aa' );
  ok( $match<bool>, "/a+/" );
  $match = $rule( '!!' );
  ok( ! $match<bool>, "rejects unmatching text" );
}

{
  $rule = 
    ruleop::concat(
      ruleop::greedy_plus( 
        ruleop::alternation( [
          ruleop::constant( 'a' ), 
          ruleop::constant( 'c' ), 
        ] ),
      ),
      ruleop::constant( 'ab' )
    );
  $match = $rule( 'aacaab' );
  ok( $match<bool>, "/[a|c]+ab/ with backtracking" );
  # print Dumper( $match );
}

