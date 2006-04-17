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
        my $whitespace_before = 0;

      for ( 1 ) {
        $m = Pugs::Grammar::BaseCategory->ws( $match );
        # <ws> is nonstandard in that it returns a hashref instead of a Match
        # print "match is ",Dumper($m),"\n";
        if ( $m->{bool} ) {
            $match = $m->{tail};
            $whitespace_before = 1;
        }
        # print "tail $match \n"; 
        $m = Pugs::Grammar::StatementControl->parse( $match, { p => 1 } );
        last if ( $m );
        if ( $match =~ /^</ ) {   # && ! $whitespace_before ) {
            # after whitespace means '<' (default)
            # without whitespace means '<str>'
            print "checking angle quote ...\n";
            $m = Pugs::Grammar::Term->angle_quoted( substr($match, 1), { p => 1 } );
            if ( $m ) {
                print "Match: ",Dumper $m->();
                if ( grep { $_ eq 'NUM' } $p->YYExpect ) {
                    # expects a term
                    $m = Pugs::Runtime::Match->new( { 
                        bool  => 1,
                        match => $m,
                        tail  => $$m->{tail},
                        capture => { angle_quoted => $m->() },
                    } );
                }
                else {
                    # expects an op
                    $m = Pugs::Runtime::Match->new( { 
                        bool  => 1,
                        match => $m,
                        tail  => $$m->{tail},
                        capture => { op => "ANGLE", angle_quoted => $m->() },
                    } );
                }
                print "Match: ",Dumper $m->();
                last;
            }
        }
        $m = Pugs::Grammar::Operator->parse( $match, { p => 1 } );
        last if ( $m );
        $m = Pugs::Grammar::Term->parse( $match, { p => 1 } );
        last if ( $m );
        #print "unrecognized token\n";
      } # /for

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

        print "expect NUM \n" if grep { $_ eq 'NUM' } $p->YYExpect;
        print "expect '/' \n" if grep { $_ eq '/' } $p->YYExpect;

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
    #print Dumper $out;
    return $out;
}

1;
