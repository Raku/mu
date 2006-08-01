# pX/Common/p6rule.t - fglock

use strict;
use warnings;

#require 'iterator_engine.pl';

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';

use_ok( 'Pugs::Runtime::Rule' );
use Pugs::Runtime::Match::Ratchet;

=for doc see Runtime::Rule
A "rule" function gets as argument a list:
0 - a string to match 
1 - an optional "continuation"
2 - a partially built match tree
3 - a leaf pointer in the match tree
4 - a grammar name
5 - pos - TODO - change #0 ???
#6 - the whole string to match - TODO - unify with $_[0]
7 - argument list - <subrule($x,$y)>
=cut

my ( $rule, $match );

{
  $rule = 
        Pugs::Runtime::Rule::constant( 'a' );
  $rule->( 'a123', undef, $match, $match, undef, 0, undef, undef );
  #print Dumper( $match );
  ok ( $match, "/a/ #1" );
  is ( $match->to, 1, "tail is ok" );
  is ( "$match", "a", "stringification" );
  $rule->( 'c123', undef, $match, $match, undef, 0, undef, undef );
  ok ( ! $match, "/c/ #2" );
  #print Dumper( $match );
}

{
  $rule = 
      Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] );
  $rule->( 'a123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/a|c/ #1" );
  is ( $match->to, 1, "tail is ok" );
  $rule->( 'c123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/a|c/ #2" );
  is ( $match->to, 1, "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
      Pugs::Runtime::Rule::concat( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] );
  $rule->( 'ac123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/ac/ #1" );
  is ( $match->to, 2, "tail is ok" );
  $rule->( 'cc123', undef, $match, $match, undef, 0, undef, undef );
  ok ( ! $match, "/ac/ #2" );
  #print Dumper( $match );
}

{
  $rule = 
        Pugs::Runtime::Rule::named( 
            'const', 
            Pugs::Runtime::Rule::constant( 'a' ) 
        );
  $rule->( 'a123', undef, $match, $match, undef, 0, undef, undef );
  #print Dumper( $match );
  ok ( $match, "/a/ #1" );
  is ( $match->{const}, 'a', "named var" );
  # print Dumper( $match );
}

{
  $rule = 
      Pugs::Runtime::Rule::concat( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::before( 
            Pugs::Runtime::Rule::constant( 'c' )
        ),
      ] );
  $rule->( 'ac123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/a<before c>/ #1" );
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
  $rule->( 'a123', undef, $match, $match, undef, 0, undef, undef );
  #print Dumper( $match );
  ok ( $match, "/[a|c]/ #1" );
  is ( $match->to, 1, "tail is ok" );
  $rule->( 'c123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/[a|c]/ #2" );
  is ( $match->to, 1, "tail is ok" );
  # print Dumper( $match );
}

{
  # alternation backtracking
  $rule = 
      Pugs::Runtime::Rule::concat( [
        Pugs::Runtime::Rule::optional( 
            Pugs::Runtime::Rule::constant( 'a' ), 
        ),
        Pugs::Runtime::Rule::constant( 'd' ), 
      ] );
  $rule->( 'ad123', undef, $match, $match, undef, 0, undef, undef );
  #print Dumper( $match );
  ok ( $match, "/a?d/ #1" );
  is ( $match->to, 2, "tail is ok" );
  $rule->( 'd123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/a?d/ #2" );
  is ( $match->to, 1, "tail is ok" );
  # print Dumper( $match );
}

{
  # concat backtracking
  $rule = 
      Pugs::Runtime::Rule::concat( [
        Pugs::Runtime::Rule::non_greedy_plus( 
          Pugs::Runtime::Rule::alternation( [
            Pugs::Runtime::Rule::constant( 'a' ), 
            Pugs::Runtime::Rule::constant( 'c' ), 
          ] ) ),
        Pugs::Runtime::Rule::constant( 'd' ), 
      ] );
  $rule->( 'ad123', undef, $match, $match, undef, 0, undef, undef );
  #print Dumper( $match );
  ok ( $match, "/[a|c]+?d/ #1" );
  is ( $match->to, 2, "tail is ok" );
  $rule->( 'acacd123', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/[a|c]+?d/ #2" );
  is ( $match->to, 5, "tail is ok" );
  # print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::greedy_star( 
      Pugs::Runtime::Rule::constant( 'a' ) 
    );
  is ( ref $rule, "CODE", "rule 'a*' is a coderef" );
  $rule->( 'aa', undef, $match, $match, undef, 0, undef, undef );
  # print Dumper( $match );
  ok ( $match, "/a*/" );
  # print Dumper( $match );
  $rule->( '', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "matches 0 occurrences" );
  # print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::greedy_plus( 
      Pugs::Runtime::Rule::constant( 'a' ) 
    );
  $rule->( 'aa', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/a+/" );
  $rule->( '!!', undef, $match, $match, undef, 0, undef, undef );
  ok ( ! $match, "rejects unmatching text" );
}

{
  $rule = 
    Pugs::Runtime::Rule::concat( [
      Pugs::Runtime::Rule::greedy_plus( 
        Pugs::Runtime::Rule::alternation( [
          Pugs::Runtime::Rule::constant( 'a' ), 
          Pugs::Runtime::Rule::constant( 'c' ), 
        ] ),
      ),
      Pugs::Runtime::Rule::constant( 'ab' )
    ] );
  $rule->( 'aacaab', undef, $match, $match, undef, 0, undef, undef );
  ok ( $match, "/[a|c]+ab/ with backtracking" );
  # print "test: ",Dumper( $match );
}


{
  $rule = 
    Pugs::Runtime::Rule::concat( [
        Pugs::Runtime::Rule::named( 
            'const1', 
            Pugs::Runtime::Rule::constant( 'a' ) 
        ),
        Pugs::Runtime::Rule::named( 
            'const2', 
            Pugs::Runtime::Rule::constant( 'b' ) 
        ),
        Pugs::Runtime::Rule::named( 
            'const3', 
            Pugs::Runtime::Rule::constant( 'c' ) 
        ),
    ] );
  $rule->( 'abc123', undef, $match, $match, undef, 0, undef, undef );
  print Dumper( $match );
  ok ( $match, "/(a)(b)(c)/ named #1" );
  is ( $match->{const}, 'a', "named var" );
  # print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::concat( [
        Pugs::Runtime::Rule::positional(  
            Pugs::Runtime::Rule::constant( 'a' ) 
        ),
        Pugs::Runtime::Rule::positional( 
            Pugs::Runtime::Rule::constant( 'b' ) 
        ),
        Pugs::Runtime::Rule::positional(  
            Pugs::Runtime::Rule::constant( 'c' ) 
        ),
    ] );
  $rule->( 'abc123', undef, $match, $match, undef, 0, undef, undef );
  print Dumper( $match );
  ok ( $match, "/(a)(b)(c)/ positional #1" );
  is ( $match->{const}, 'a', "named var" );
  # print Dumper( $match );
}

__END__

{
  $rule = 
    Pugs::Runtime::Rule::non_greedy_plus( 
      Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::constant( 'a' ), 
        Pugs::Runtime::Rule::constant( 'c' ), 
      ] ),
    );
  $rule->( '', undef, $match, $match, undef, 0, undef, undef );
  ( $stat, $assertion, $match, $tail ) = $rule->( 'aacaab', undef, {capture=>1} );
  ok ( defined $match, "/[a|c]+/" );
  is ( $tail, 'acaab', "tail is ok" );
  #print Dumper( $match );
}

{
  $rule = 
    Pugs::Runtime::Rule::concat( [
      Pugs::Runtime::Rule::non_greedy_plus( 
        Pugs::Runtime::Rule::alternation( [
          Pugs::Runtime::Rule::constant( 'a' ), 
          Pugs::Runtime::Rule::constant( 'c' ), 
        ] ),
      ),
      Pugs::Runtime::Rule::constant( 'cb' )
    ] );
  ( $stat, $assertion, $match, $tail ) = $rule->( 'aacacb' );
  ok ( defined $match, "/[a|c]+?ab/ with backtracking" );
  #print Dumper( $match );
}

{
  # tests for a problem found in the '|' implementation in p6rule parser
  
  my $rule = 
    Pugs::Runtime::Rule::constant( 'a' );
  my $alt = 
    Pugs::Runtime::Rule::concat( [
        $rule,
        Pugs::Runtime::Rule::optional (
            Pugs::Runtime::Rule::concat( [
                Pugs::Runtime::Rule::constant( '|' ),
                $rule
            ] )
        )
    ] );
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
