# pX/Common/p6rule.t - fglock

use strict;
use warnings;

#require 'iterator_engine.pl';

use Test::More tests => 22;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';

use_ok( 'Pugs::Runtime::Rule' );
use Pugs::Runtime::Match::Ratchet;

my ( $rule, $match );

{
  $rule = Pugs::Runtime::Rule::constant( 'a' );
  $rule->( 'a123', undef, {capture=>1}, $match );
  #print Dumper( $match );
  ok ( $match->bool, "a =~ /a/ #1" );
  is ( $match->tail, '123', "tail is ok" );
  $rule->( 'c', undef, {capture=>1}, $match );
  ok ( ! $match->bool, "c =~ /a/ #2" );
  #is ( $match->tail, 'c123', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::non_greedy_plus( 
      Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] ),
    );
  $rule->( 'a123', undef, {capture=>1}, $match );
  ok ( $match->bool, "/[a|c]/ #1" );
  is ( $match->tail, '123', "tail is ok" );
  $rule->( 'c123', undef, {capture=>1}, $match );
  ok ( $match->bool, "/[a|c]/ #2" );
  is ( $match->tail, '123', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::greedy_star( 
      Pugs::Runtime::Rule::constant( 'a' ) 
    );
  is ( ref $rule, "CODE", "rule 'a*' is a coderef" );
  $rule->( 'aa', undef, {}, $match );
  #print Dumper( $match );
  ok ( $match->bool, "/a*/" );
  #print Dumper( $match );
  $rule->( '', undef, {}, $match );
  ok ( $match->bool, "matches 0 occurrences" );
  #print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::greedy_plus( 
      Pugs::Runtime::Rule::constant( 'a' ) 
    );
  $rule->( 'aa', undef, {}, $match );
  ok ( $match->bool, "/a+/" );
  $rule->( '!!', undef, {}, $match );
  ok ( ! $match->bool, "rejects unmatching text" );
}

{
  $rule = 
    Pugs::Runtime::Rule::concat( 
      Pugs::Runtime::Rule::greedy_plus( 
        Pugs::Runtime::Rule::alternation( [
          Pugs::Runtime::Rule::constant( 'a' ), 
          Pugs::Runtime::Rule::constant( 'c' ), 
        ] ),
      ),
      Pugs::Runtime::Rule::constant( 'ab' )
     );
  $rule->( 'aacaab', undef, {}, $match );
  ok ( $match->bool, "/[a|c]+ab/ with backtracking" );
  # print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::non_greedy_plus( 
      Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] ),
    );
  $rule->( 'aacaab', undef, {capture=>1}, $match );
  ok ( $match, "/[a|c]+/" );
  is ( $match->tail, 'acaab', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::concat(
      Pugs::Runtime::Rule::non_greedy_plus( 
        Pugs::Runtime::Rule::alternation( [
          Pugs::Runtime::Rule::constant( 'a' ), 
          Pugs::Runtime::Rule::constant( 'c' ), 
        ] ),
      ),
      Pugs::Runtime::Rule::constant( 'cb' )
    );
  $rule->( 'aacacb', undef, {capture=>1}, $match );
  ok ( defined $match, "/[a|c]+?ab/ with backtracking" );
  #print Dumper( $match );
}

{
  # tests for a problem found in the '|' implementation in p6rule parser
  
  my $rule = 
    Pugs::Runtime::Rule::constant( 'a' );
  my $alt = 
    Pugs::Runtime::Rule::concat(
        $rule,
        Pugs::Runtime::Rule::optional (
            Pugs::Runtime::Rule::concat(
                Pugs::Runtime::Rule::constant( '|' ),
                $rule
            )
        )
    );
  $alt->( 'a', undef, {capture=>1}, $match );
  ok ( defined $match, "/a|a/ #1" );
  $alt->( 'a|a', undef, {capture=>1}, $match );
  ok ( defined $match, "/a|a/ #2" );

  # adding '*' caused a deep recursion error (fixed)

  $alt = 
    Pugs::Runtime::Rule::concat(
        $rule,
        Pugs::Runtime::Rule::greedy_star(
          Pugs::Runtime::Rule::concat(
              Pugs::Runtime::Rule::constant( '|' ),
              $rule
          )
        )
    );
  $alt->( 'a', undef, {capture=>1}, $match );
  ok ( $match, "/a [ |a ]*/ #1" );
  $alt->( 'a|a', undef, {capture=>1}, $match );
  ok ( $match, "/a [ |a ]*/ #2" );
  $alt->( 'a|a|a', undef, {capture=>1}, $match );
  ok ( $match, "/a [ |a ]*/ #3" );

}

