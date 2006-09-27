# pX/Common/p6rule.t - fglock

use strict;
use warnings;

use Test::More tests => 28;
# use Data::Dumper;
# $Data::Dumper::Indent = 1;
# $Data::Dumper::Pad = '# ';

use_ok( 'Pugs::Runtime::Regex' );
use Pugs::Runtime::Match;

my ( $rule, $match );

{
  $rule = Pugs::Runtime::Regex::constant( 'a' );
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
    Pugs::Runtime::Regex::non_greedy_plus( 
      Pugs::Runtime::Regex::alternation( [
        Pugs::Runtime::Regex::constant( 'a' ), 
        Pugs::Runtime::Regex::constant( 'c' ), 
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
  # -- continuations in alternation()
  $rule = 
      Pugs::Runtime::Regex::alternation( [
        Pugs::Runtime::Regex::constant( 'a' ), 
        Pugs::Runtime::Regex::constant( 'ab' ), 
      ] );
  $rule->( 'ab', undef, {}, $match );
  #print "state: ", Dumper($match->state), "\n";
  is ( $match->str, 'a', "/[a|ab]/ multi-match continuation state #0" );
  $rule->( 'ab', $match->state, {}, $match );
  #print "state: ", Dumper($match->state), "\n";
  is ( $match->str, 'ab', "/[a|ab]/ multi-match continuation state #1" );
  #$rule->( 'ab', $match->state, {}, $match );
  #print "state: ", Dumper($match->state), "\n";
  #is ( $match->str, '', "/[a|ab]/ multi-match state #2" );
  #print Dumper( $match );
}

{
  # -- continuations in concat()
  $rule = 
    Pugs::Runtime::Regex::concat( [
      Pugs::Runtime::Regex::alternation( [
        Pugs::Runtime::Regex::constant( 'a' ), 
        Pugs::Runtime::Regex::constant( 'ab' ), 
      ] ),
      Pugs::Runtime::Regex::alternation( [
        Pugs::Runtime::Regex::constant( 'b' ), 
        Pugs::Runtime::Regex::constant( 'bb' ), 
      ] ),
    ] );
  my $str = 'abbb';
  $rule->( $str, undef, {}, $match );
  #print "state 1: ", Dumper($match->state), "\n";
  is ( $match->str, 'ab', "/[a|ab][b|bb]/ continuation state #0" );

  $rule->( $str, $match->state, {}, $match );
  #print "state 2: ", Dumper($match->state), "\n";
  is ( $match->str, 'abb', "state #1" );

  $rule->( $str, $match->state, {}, $match );
  #print "state 3: ", Dumper($match->state), "\n";
  is ( $match->str, 'abb', "state #2" );

  $rule->( $str, $match->state, {}, $match );
  #print "state 4: ", Dumper($match->state), "\n";
  is ( $match->str, 'abbb', "state #3" );

}

{
  $rule = 
    Pugs::Runtime::Regex::greedy_star( 
      Pugs::Runtime::Regex::constant( 'a' ) 
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
    Pugs::Runtime::Regex::greedy_plus( 
      Pugs::Runtime::Regex::constant( 'a' ) 
    );
  $rule->( 'aa', undef, {}, $match );
  ok ( $match->bool, "/a+/" );
  $rule->( '!!', undef, {}, $match );
  ok ( ! $match->bool, "rejects unmatching text" );
}

{
  $rule = 
    Pugs::Runtime::Regex::concat( 
      Pugs::Runtime::Regex::greedy_plus( 
        Pugs::Runtime::Regex::alternation( [
          Pugs::Runtime::Regex::constant( 'a' ), 
          Pugs::Runtime::Regex::constant( 'c' ), 
        ] ),
      ),
      Pugs::Runtime::Regex::constant( 'ab' )
     );
  $rule->( 'aacaab', undef, {}, $match );
  ok ( $match->bool, "/[a|c]+ab/ with backtracking" );
  # print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Regex::non_greedy_plus( 
      Pugs::Runtime::Regex::alternation( [
        Pugs::Runtime::Regex::constant( 'a' ), 
        Pugs::Runtime::Regex::constant( 'c' ), 
      ] ),
    );
  $rule->( 'aacaab', undef, {capture=>1}, $match );
  ok ( $match, "/[a|c]+/" );
  is ( $match->tail, 'acaab', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Regex::concat(
      Pugs::Runtime::Regex::non_greedy_plus( 
        Pugs::Runtime::Regex::alternation( [
          Pugs::Runtime::Regex::constant( 'a' ), 
          Pugs::Runtime::Regex::constant( 'c' ), 
        ] ),
      ),
      Pugs::Runtime::Regex::constant( 'cb' )
    );
  $rule->( 'aacacb', undef, {capture=>1}, $match );
  ok ( defined $match, "/[a|c]+?ab/ with backtracking" );
  #print Dumper( $match );
}

{
  # tests for a problem found in the '|' implementation in p6rule parser
  
  my $rule = 
    Pugs::Runtime::Regex::constant( 'a' );
  my $alt = 
    Pugs::Runtime::Regex::concat(
        $rule,
        Pugs::Runtime::Regex::optional (
            Pugs::Runtime::Regex::concat(
                Pugs::Runtime::Regex::constant( '|' ),
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
    Pugs::Runtime::Regex::concat(
        $rule,
        Pugs::Runtime::Regex::greedy_star(
          Pugs::Runtime::Regex::concat(
              Pugs::Runtime::Regex::constant( '|' ),
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

