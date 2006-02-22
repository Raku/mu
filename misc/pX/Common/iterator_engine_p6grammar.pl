# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of a grammar that could parse pge/P6Rule.grammar

use strict;
use warnings;

require 'iterator_engine_p6rule.pl';

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';

{
  package grammar1;

  my ( $program );
  my ( $stat, $match, $tail );
  my $rule = \&rule;

  no warnings 'once';

    ( $stat, $match, $tail ) = $rule->( '\=[pod|head1] .*? \=cut' );
    $program = main::emit_rule( $match );
    # print "program:\n$program";
  *pod = ruleop::label( 'pod' , eval($program) ); die $@ if $@;

    ( $stat, $match, $tail ) = $rule->( 'grammar .*? \;' );
    $program = main::emit_rule( $match );
    # print "program:\n$program";
  *grammar_name = ruleop::label( 'grammar_name' , eval($program) ); die $@ if $@;

    ( $stat, $match, $tail ) = 
        $rule->( 'rule <ws>+ <word> <ws>* \{ <rule> \}' );
    $program = main::emit_rule( $match );
    # print "program:\n$program";
  *rule_decl = ruleop::label( 'rule_decl' , eval($program) ); die $@ if $@;

    ( $stat, $match, $tail ) = 
        $rule->( '[<ws>*[<pod>|<grammar_name>|<rule_decl>]]*<ws>*' );
    $program = main::emit_rule( $match );
    # print "program:\n$program";
  *grammar = eval($program); die $@ if $@;

}

# ------ grammar emitter

my $namespace = 'grammar1::';

{
  package grammar;
  use Data::Dumper; # import Dumper

sub emit_rule {
    my $n = $_[0];
    local $Data::Dumper::Indent = 0;
    #print "emit_rule: ", ref($n)," ",Dumper( $n ), "\n";
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            push @s, emit_rule( $_ );
        }
        # XXX
        return $s[0] unless $s[1];
        return $s[1] unless $s[0];
        return $s[0] . $s[1] ;
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        #print "$k => $v \n";

        if ( $k eq 'pod' ) {
            return;
        }
        if ( $k eq 'grammar_name' ) {
            return "# TODO package [insert grammar name here];\n";
        }
        if ( $k eq 'rule_decl' ) {
            local $Data::Dumper::Indent = 1;
            #print "*** rule_decl:\n",Dumper $v;
            return "# TODO sub { ... }\n";
        }

        # other nodes (unused?)

        elsif ( $k eq 'ws' ) {
            return;
        }
        elsif ( $k eq 'rule' ) {
            return "\\&{'$namespace$v'}\n";
        }
        elsif ( $k eq 'word' ) {
            return "ruleop::constant( '$v' )\n";
        }
        else {
            die "unknown node: ", Dumper( $n );
        }
    }
    else 
    {
        die "unknown node: ", Dumper( $n );
    }
}

} # /package

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
rule xxx {xxx}
EOT
  ok ( defined $match, "grammar" );
  ok ( ! $tail, "full match $tail" );
  #print "grammar:\n", Dumper $match;

  my $program = grammar::emit_rule( $match );
  print "program: \n", $program;
