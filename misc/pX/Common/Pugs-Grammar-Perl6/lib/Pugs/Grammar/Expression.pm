package Pugs::Grammar::Expression;

use strict;
use warnings;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;
use Pugs::Grammar::StatementControl;
use base 'Pugs::Grammar::Base';

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

# XXX - PCR is not calling this
*ws = &Pugs::Grammar::BaseCategory::ws;

our $parser = Pugs::Compiler::Rule->compile( q!
        .*
        { 
            return Pugs::Grammar::Expression::ast( $/ ); 
        }
    !);

*parse = $parser->code;

sub ast {
    my $match = shift;
    $match .= '';
    # print "Grammar::Expression::AST '$match' \n";
    my $p;
    my $last = length( $match );
    my $lex = sub {

        my $m;
        $m = Pugs::Grammar::BaseCategory->ws( $match );
        # ws is nonstandard in that it returns a hashref instead of a Match
        # print "match is ",Dumper($m),"\n";
        $match = $m->{tail} if $m->{bool};
        # print "tail $match \n"; 
        $m = Pugs::Grammar::StatementControl->parse( $match, { p => 1 } );
        if ( $m ) {
            #print "statement-control: ", Dumper $m->();
        }
        else {
          $m = Pugs::Grammar::Operator->parse( $match, { p => 1 } );
          if ( $m ) {
              #print "op: ", Dumper $m->();
          }
          else {
            $m = Pugs::Grammar::Term->parse( $match, { p => 1 } );
            if ( $m ) {
                #print "term: ", Dumper $m->();
            }
            else {
                #print "unrecognized token\n";
            }
          }
        }
        $match = $$m->{tail};
        my $ast = $m->();
        $ast->{pos} = $last - length( $match );
        my $t;
        if ( exists $ast->{stmt} ) {
            $t = [ $ast->{stmt} => $ast ]
        }
        elsif ( exists $ast->{op} ) {
            $t = [ $ast->{op} => $ast ]
        }
        elsif ( exists $ast->{bareword} ) {
            $t = [ 'BAREWORD' => $ast ]
        }
        else {
            $t = [ 'NUM' => $ast ]
        }
        $t=['',''] unless $match; # defined($t);
        print "token: $$t[0] ", Dumper( $$t[1] );
        # print "expect: ", Dumper( $p->YYExpect );
        return($$t[0],$$t[1]);
    };

    # TODO - check for remaining whitespace!

    $p = Pugs::Grammar::Operator->new(
        yylex => $lex, 
        yyerror => sub { warn "error in expression: $match ..." },
    );

    my $out=$p->YYParse;
    print Dumper $out;
    return $out;
}

1;
