# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see also: ../../Grammars/rx_grammar.pm

use strict;
use warnings;

# require 'iterator_engine.pl';
require 'iterator_engine_p6rule.pl';

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';
my ( $program, $compiled );
my ( $stat, $assertion, $match, $tail );
my $rule = \&grammar1::rule;

{
  $match = $rule->( '<word>' );
  #print Dumper( $match );
  ok ( $match->{bool}, "parse rule" );
  $program = emit_rule( $match->{capture} );
  # print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  $match = $compiled->( '!some_word' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
}

{
  $match = $rule->( '..' );
  ok ( $match->{bool}, "parse rule - dot-dot" );
  #print Dumper( $match );
  $program = emit_rule( $match->{capture} );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  $match = $compiled->( 'some_word' );
  #print Dumper( $match );
  ok ( $match->{bool}, "parse sample of text" );

  ( $stat, $assertion, $match, $tail ) = $compiled->( '!' );
  ok ( ! $match->{bool}, "rejects unmatching text" );
}

print "# XXX tests removed - API change\n";
__END__

{
  ( $stat, $assertion, $match, $tail ) = $rule->( '<word> <ws>' );
  ok ( defined $match, "parse rule - 2 terms, with whitespace" );
  $program = emit_rule( $match->{capture} );
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );
  is ( $tail, 'other', "remaining unmatched text (tail)" );

  ( $stat, $assertion, $match, $tail ) = $compiled->( 'one_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( '<word> <ws>*', undef, {capture=>1} );
  ok ( $assertion, "parse rule - 2 terms, with star" );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( $assertion, "parse sample of text" );
  is ( $tail, 'other', "remaining unmatched text (tail)" );

  # this test doesn't apply
  #( $stat, $assertion, $match, $tail ) = $compiled->( 'one_word!' );
  #ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( 'a+?', undef, {capture=>1} );
  ok ( defined $match, "parse rule - a+?" );
  $program = emit_rule( $match );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'aaaasome_word' );
  # print Dumper( $match );
  ok ( $assertion, "parse sample of text" );
  is ( $tail, 'aaasome_word', "correct left-out" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( 'a?', undef, {capture=>1} );
  ok ( defined $match, "parse rule - a?" );
  $program = emit_rule( $match );
  #print "program:\n$program";
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'aaa' );
  # print Dumper( $match );
  ok ( $assertion, "parse sample 1" );
  is ( $tail, 'aa', "correct left-out" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'bbb' );
  # print Dumper( $match );
  ok ( $assertion, "parse sample 2" );
  is ( $tail, 'bbb', "correct left-out" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( '(<word>) <ws>' );
  ok ( defined $match, "parse rule - 2 terms, with capture" );
  $program = emit_rule( $match );
  #print $program;
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );

  # TODO - test captured text

  ( $stat, $assertion, $match, $tail ) = $compiled->( 'one_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( '<word> [ <ws> <word> ]' );
  ok ( defined $match, "parse rule - non-capturing group" );
  $program = emit_rule( $match );
  #print $program;
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );

  ( $stat, $assertion, $match, $tail ) = $compiled->( '!some_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( '<word>|<ws>' );
  ok ( defined $match, "parse rule - alternates" );
  #print "# match:\n", Dumper( $match );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  # print "# program: $program";
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 1" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( ' ' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 2" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( '!' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $assertion, $match, $tail ) = $rule->( '<word>|<ws>|.' );
  ok ( defined $match, "parse rule - 3 alternates" );
  ok ( ! $tail, "full match" );
  #print "# match:\n", Dumper( $match );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  #print "# program: $program";
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 1" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( ' ' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 2" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( '-' );
  ok ( defined $match, "parse sample 3" );
}

{

  # XXX - allow whitespace everywhere
  # ( $stat, $assertion, $match, $tail ) = $rule->( '<word>| [ <ws> <word> ]' );

  ( $stat, $assertion, $match, $tail ) = $rule->( '<word>|[<ws><word>]' );
  ok ( defined $match, "parse rule - alternates with grouping" );
  ok ( ! $tail, "full match" );
  #print "# match:\n", Dumper( $match );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  #print "# program: $program";
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 1" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( ' other_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 2" );
  ( $stat, $assertion, $match, $tail ) = $compiled->( '!' );
  ok ( !defined $match, "rejects unmatching text" );
}

__END__

# TODO - test backtracking (implement '*' first)

# TODO - convert older tests to Test::More

print Dumper ruleop::rule()->( 0, '{ print 1+1, "\n" }' )
                     ->( 0, '' );

print Dumper ruleop::rule( 0, ' ' )
                     ->( 0, 'x' );

print Dumper ruleop::rule( 0, '<word>' )
                     ->( 0, ' abc def' );

print Dumper ruleop::rule( 0, 'abc' )
                     ->( 0, 'abc' );
print Dumper ruleop::rule( 0, '<word>' )
                     ->( 0, 'abc' );

__END__

=for later

$state = 0;
{
    while ( defined $state ) {
        ($state, $tmp, undef) = 
            ruleop::greedy_plus( rule::constant( 'ab' ) )
            ->( $state, qw(a b a b a b) );
        print "recursive\n", Dumper( $tmp );
    }
}

__END__

=for tested

$state = 0;
{
    my $alt = rule::alternation( 
        rule::constant('ab'), rule::constant('ba'), rule::constant('a') );
    while ( defined $state ) {
        ($state, $tmp, undef) = rule::concat( 
                $alt, rule::optional( $alt ) 
            )->( $state, qw(a b a)
        );
        print "concat\n", Dumper( $tmp );
    }
}

$state = 0;
while ( defined $state ) {
    print "alternation\n", Dumper( 
        ($state, $tmp, undef) = rule::alternation( 
            rule::constant('ab'), rule::constant('a') 
        )->( $state, qw(a b c) ) 
    );
}

$state = 0;
while ( defined $state ) {
    print "greedy\n", Dumper( 
        ($state, $tmp, undef) = rule::greedy( rule::constant('a') )->( $state, qw(a a b c) ) 
    );
}

$state = 0;
while ( defined $state ) {
    print "greedy + alternation state\n", Dumper( 
        ($state, $tmp, undef) = rule::greedy( 
            rule::alternation( 
                rule::constant('ab'), rule::constant('a'), 
                rule::constant('cd'), rule::constant('ba'), 
                rule::constant('bc'), )
        )->( $state, qw(a b a b c) ) 
    );
}

=cut

__END__

$state = 0;
for ( 0 .. 3 ) {
    print "greedy + alternation state $_\n", Dumper( 
        ($state, undef, undef) = rule::greedy( 
            rule::alternation( 
                rule::constant('cd'), rule::constant('ab'), 
                rule::constant('a'),  rule::constant('cd') )
        )->( $state, qw(a b a b c) ) 
    );
}

__END__

print "any-char\n", Dumper( 
  &{'rule::.'}( 0, qw( a b ) ) 
);

print "greedy\n", Dumper( 
  rule::greedy( rule::constant('a') )->( 0, qw(a a a b c) ) 
);
print "greedy backtrack\n", Dumper( 
  rule::concat( 
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'} 
  )->( 0, qw(a a a a) ) 
);
print "greedy no-match\n", Dumper( 
  rule::concat(
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'}
  )->( 0, qw(b a a a a) ) 
);
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
