# pX/Common/iterator_engine_p6regex.t - fglock
#
# experimental implementation of a grammar that could parse pge/P6Rule.grammar

use strict;
use warnings;

use Test::More qw(no_plan);

require 'iterator_engine_p6grammar.pl';

my $match;

{
  $match = grammar1::pod( 
    "=pod\n".
    "some text\n".
    "=cut" );
  ok ( $match->{bool}, "pod" );
  #print "pod:\n", Dumper $match;
}

{
  $match = grammar1::grammar_name( 
    "grammar PGE::P6Rule;" );
  ok ( $match->{bool}, "grammar name" );
  #print "grammar_name:\n", Dumper $match;
}

{
  $match = grammar1::rule( 
    "rule identifier {const <word>}" );
  ok ( $match->{bool}, "rule" );
  #print "rule:\n", Dumper $match;
}

{
  $match = grammar1::grammar( <<EOT );
=pod 
  test
=cut
grammar test;
rule xxx {xxx}
EOT
  ok ( $match->{bool}, "grammar" );
  is ( $match->{tail}, '', "full match" );
  #print "grammar:\n", Dumper $match;

  my $program = grammar::emit_rule( $match->{capture} );
  print "program: \n", grammar::header() . $program;
}

{
  open( FILE, 'iterator_engine_p6rule_grammar.p6' );
  my $text;
  { local $/; $text = <FILE> }
  #print $text;
  $match = grammar1::grammar( $text );
  ok ( $match->{bool}, "grammar was parsed from file" );
  is ( $match->{tail}, '', "full match" );
  #print "rule:\n", Dumper $match->{capture};
  my $program = grammar::emit_rule( $match->{capture} );
  print "program: \n", grammar::header() . $program;
}
