# pX/Common/p6rule.t - fglock

use strict;
use warnings;

#require 'iterator_engine.pl';

use Test::More qw(no_plan);
use Data::Dump::Streamer;

use_ok( 'Pugs::Runtime::Rule' );

my ( $rule, $match );

{
  $rule = 
    Pugs::Runtime::Rule::non_greedy_plus( 
      Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] ),
    );
  $match = $rule->( 'a123', undef, {capture=>1} );
  ok ( $match->{bool}, "/[a|c]/ #1" );
  is ( $match->{tail}, '123', "tail is ok" );
  $match = $rule->( 'c123', undef, {capture=>1} );
  ok ( $match->{bool}, "/[a|c]/ #2" );
  is ( $match->{tail}, '123', "tail is ok" );
  #print Dump( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::greedy_star( 
      Pugs::Runtime::Rule::constant( 'a' ) 
    );
  is ( ref $rule, "CODE", "rule 'a*' is a coderef" );
  $match = $rule->( 'aa' );
  # print Dump( $match );
  ok ( $match->{bool}, "/a*/" );
  #print Dump( $match );
  $match = $rule->( '' );
  ok ( $match->{bool}, "matches 0 occurrences" );
  #print Dump( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::greedy_plus( 
      Pugs::Runtime::Rule::constant( 'a' ) 
    );
  $match = $rule->( 'aa' );
  ok ( $match->{bool}, "/a+/" );
  $match = $rule->( '!!' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
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
  $match = $rule->( 'aacaab' );
  ok ( $match->{bool}, "/[a|c]+ab/ with backtracking" );
  # print Dump( $match );
}

print "# XXX other tests disabled due to a big API change\n";
__END__

{
  $rule = 
    Pugs::Runtime::Rule::non_greedy_plus( 
      Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] ),
    );
  ( $stat, $assertion, $match, $tail ) = $rule->( 'aacaab', undef, {capture=>1} );
  ok ( defined $match, "/[a|c]+/" );
  is ( $tail, 'acaab', "tail is ok" );
  #print Dump( $match );
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
  ( $stat, $assertion, $match, $tail ) = $rule->( 'aacacb' );
  ok ( defined $match, "/[a|c]+?ab/ with backtracking" );
  #print Dump( $match );
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
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a' );
  ok ( defined $match, "/a|a/ #1" );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a|a' );
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
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a' );
  ok ( defined $match, "/a [ |a ]*/ #1" );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a|a' );
  ok ( defined $match, "/a [ |a ]*/ #2" );
  ( $stat, $assertion, $match, $tail ) = $alt->( 'a|a|a' );
  ok ( defined $match, "/a [ |a ]*/ #3" );

}

__END__

# old tests 

print "word\n", Dump( 
  &{'rule::<word>'}( 0, qw(b a a ! !) ) 
);
print "word concat\n", Dump( 
  rule::concat( \&{'rule::<word>'}, \&{'rule::<ws>'} )->( 0, qw(b a ),' ' ) 
);
print "non_greedy + backtracking\n", Dump( 
  rule::concat(
    rule::non_greedy( rule::constant('a') ),
    rule::constant('ab')
  )->( 0, qw(a a a a b) ) 
);
print "alternation + backtracking\n", Dump( 
  rule::concat(
    rule::alternation( rule::constant('a'), rule::constant('ab') ),
    rule::constant('ab')
  )->( 0, qw(a b a b) ) 
);
print "alternation + greedy + backtracking -- (ab,a,ab)(ab)\n", Dump( 
  rule::concat(
    rule::greedy(
      rule::alternation( rule::constant('a'), rule::constant('ab') )
    ),
    rule::constant('ab')
  )->( 0, qw(a b a a b a b) ) 
);
