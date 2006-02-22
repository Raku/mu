# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of a grammar that could parse pge/P6Rule.grammar

use strict;
use warnings;

require 'iterator_engine_p6regex.pl';

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';

{
  package grammar1;

  my ( $program );
  my ( $stat, $match, $tail );
  my $rule = \&rule;

  # XXX - this use of .* is wrong!

    ( $stat, $match, $tail ) = $rule->( '\=[pod|head1] .* \=cut' );
    $program = main::emit_rule( $match );
    # print "program:\n$program";
  *pod = eval($program);

    ( $stat, $match, $tail ) = $rule->( 'grammar .* \;' );
    $program = main::emit_rule( $match );
    print "program:\n$program";
  *grammar_name = eval($program);

    ( $stat, $match, $tail ) = 
        $rule->( 'rule <ws> <ws>* <word> <ws>* \{ <rule> \}' );
    $program = main::emit_rule( $match );
    print "program:\n$program";
  *rule_decl = eval($program);

    ( $stat, $match, $tail ) = 
        $rule->( '[<ws>*[<pod>|<grammar_name>|<rule_decl>]]*<ws>*' );
    $program = main::emit_rule( $match );
    print "program:\n$program";
  *grammar = eval($program);

}

# ------ tests

use Test::More qw(no_plan);
my ( $stat, $match, $tail );

  ( $stat, $match, $tail ) = grammar1::pod( 
    "=pod\n".
    "some text\n".
    "=cut" );
  ok ( defined $match, "pod" );
  #print "pod:\n", Dumper $match;

  ( $stat, $match, $tail ) = grammar1::grammar_name( 
    "grammar PGE::P6Rule;" );
  ok ( defined $match, "grammar name" );
  #print "grammar_name:\n", Dumper $match;

  ( $stat, $match, $tail ) = grammar1::rule( 
    "rule identifier {const <word>}" );
  ok ( defined $match, "rule" );
  #print "rule:\n", Dumper $match;

  ( $stat, $match, $tail ) = grammar1::grammar( <<EOT );
=pod 
  test
=cut
grammar test;
rule xxx {xxx};
EOT
  ok ( defined $match, "grammar" );
  ok ( ! $tail, "full match" );
  print "grammar:\n", Dumper $match;

