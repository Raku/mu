# pX/Common/iterator_engine_p6regex.t - fglock

use strict;
use warnings;

require 'iterator_engine.pl';

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';
my $state;
my $tmp;
my ( $rule );
my ( $stat, $match, $tail );

{
  $rule = 
    ruleop::non_greedy_plus( 
      ruleop::alternation( [
        ruleop::constant( 'a' ), 
        ruleop::constant( 'c' ), 
      ] ),
    );
  ( $stat, $match, $tail ) = $rule->( 'a123' );
  ok ( defined $match, "/[a|c]/ #1" );
  is ( $tail, '123', "tail is ok" );
  ( $stat, $match, $tail ) = $rule->( 'c123' );
  ok ( defined $match, "/[a|c]/ #2" );
  is ( $tail, '123', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::greedy_star( 
      ruleop::constant( 'a' ) 
    );
  is ( ref $rule, "CODE", "rule 'a*' is a coderef" );
  ( $stat, $match, $tail ) = $rule->( 'aa' );
  # print Dumper( $match );
  ok ( defined $match, "/a*/" );
  ( $stat, $match, $tail ) = $rule->( '' );
  ok ( defined $match, "matches 0 occurrences" );
}

{
  $rule = 
    ruleop::greedy_plus( 
      ruleop::constant( 'a' ) 
    );
  ( $stat, $match, $tail ) = $rule->( 'aa' );
  ok ( defined $match, "/a+/" );
  ( $stat, $match, $tail ) = $rule->( '!!' );
  ok ( !defined $match, "rejects unmatching text" );
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
  ( $stat, $match, $tail ) = $rule->( 'aacaab' );
  ok ( defined $match, "/[a|c]+ab/ with backtracking" );
  # print Dumper( $match );
}

{
  $rule = 
    ruleop::non_greedy_plus( 
      ruleop::alternation( [
        ruleop::constant( 'a' ), 
        ruleop::constant( 'c' ), 
      ] ),
    );
  ( $stat, $match, $tail ) = $rule->( 'aacaab' );
  ok ( defined $match, "/[a|c]+/" );
  is ( $tail, 'acaab', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    ruleop::concat(
      ruleop::non_greedy_plus( 
        ruleop::alternation( [
          ruleop::constant( 'a' ), 
          ruleop::constant( 'c' ), 
        ] ),
      ),
      ruleop::constant( 'cb' )
    );
  ( $stat, $match, $tail ) = $rule->( 'aacacb' );
  ok ( defined $match, "/[a|c]+?ab/ with backtracking" );
  #print Dumper( $match );
}

{
  # tests for a problem found in the '|' implementation in p6rule parser
  
  my $rule = 
    ruleop::constant( 'a' );
  my $alt = 
    ruleop::concat(
        $rule,
        ruleop::optional (
            ruleop::concat(
                ruleop::constant( '|' ),
                $rule
            )
        )
    );
  ( $stat, $match, $tail ) = $alt->( 'a' );
  ok ( defined $match, "/a|a/ #1" );
  ( $stat, $match, $tail ) = $alt->( 'a|a' );
  ok ( defined $match, "/a|a/ #2" );

  # adding '*' caused a deep recursion error (fixed)

  $alt = 
    ruleop::concat(
        $rule,
        ruleop::greedy_star(
          ruleop::concat(
              ruleop::constant( '|' ),
              $rule
          )
        )
    );
  ( $stat, $match, $tail ) = $alt->( 'a' );
  ok ( defined $match, "/a [ |a ]*/ #1" );
  ( $stat, $match, $tail ) = $alt->( 'a|a' );
  ok ( defined $match, "/a [ |a ]*/ #2" );
  ( $stat, $match, $tail ) = $alt->( 'a|a|a' );
  ok ( defined $match, "/a [ |a ]*/ #3" );

}

__END__

# old tests 

print "word\n", Dumper( 
  &{'rule::<word>'}( 0, qw(b a a ! !) ) 
);
print "word concat\n", Dumper( 
  rule::concat( \&{'rule::<word>'}, \&{'rule::<ws>'} )->( 0, qw(b a ),' ' ) 
);
print "non_greedy + backtracking\n", Dumper( 
  rule::concat(
    rule::non_greedy( rule::constant('a') ),
    rule::constant('ab')
  )->( 0, qw(a a a a b) ) 
);
print "alternation + backtracking\n", Dumper( 
  rule::concat(
    rule::alternation( rule::constant('a'), rule::constant('ab') ),
    rule::constant('ab')
  )->( 0, qw(a b a b) ) 
);
print "alternation + greedy + backtracking -- (ab,a,ab)(ab)\n", Dumper( 
  rule::concat(
    rule::greedy(
      rule::alternation( rule::constant('a'), rule::constant('ab') )
    ),
    rule::constant('ab')
  )->( 0, qw(a b a a b a b) ) 
);
