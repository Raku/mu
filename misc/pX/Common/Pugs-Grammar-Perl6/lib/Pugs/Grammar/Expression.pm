package Pugs::Grammar::Expression;

use strict;
use warnings;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;
use base 'Pugs::Grammar::Base';

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

our $parser = Pugs::Compiler::Rule->compile( q(
        # { print "Grammar::Expression::parse $_[0] \n";
        #  # print (keys %Pugs::Grammar::Term::hash),"\n"; 
        # }
        
        <?ws>?
        ( 
            #{ print "trying term\n"; } 
            %Pugs::Grammar::Term::hash     <?ws>?
            #{ print "term\n"; } 
        |
            #{ print "trying op\n"; } 
            %Pugs::Grammar::Operator::hash <?ws>? 
            #{ print "op\n";   } 
        )*
        <before  \} | \{ | $ >   # XXX
        
        { 
            # print "Grammar::Expression::parse returning \n";
            return Pugs::Grammar::Expression::ast( $/ ); 
        }
        
        # | { print "fail $_[0]\n"; }
    ));

*parse = $parser->code;

sub ast {
    my $match = shift;

    # print "Grammar::Expression::AST $match \n";

    #print $rule->perl5;
    # my $match = $exp->match( q(10 + $a / "abc") );
    #print Dumper( $match->[0] );
    my $m = $match->[0];
    return unless defined $m;
    my @m = @$m;
    #print Dumper( @m );
    #is( join(';', map { $_->() } @m), q(10 ;+ ;$a ;/ ;"abc"), 'split on terms' );

    my @in;
    for my $term ( @m ) {
        #print $term->(), "\n";
        #print Dumper ${$term}->{match}{match}[0]{match}[1]{capture};
        my $ast = ${$term}->{match}{match}[0]{match}[1]{capture};
        my ($type) = keys %$ast;
        #print "type: $type\n";
        if ( $type eq 'op' ) {
            push @in, [ $ast->{$type} => $ast ]
        }
        else {
            push @in, [ 'NUM' => $ast ]
        }
    }
    #print Dumper @in;

    my($lex) = sub {
        my($t)=shift(@in);
        $t=['',''] unless defined($t);
        # print "token: $$t[0]\n";
        return($$t[0],$$t[1]);
    };
    my $p = Pugs::Grammar::Operator->new(yylex => $lex, yyerror => sub { die "error in expression" });

    my $out=$p->YYParse;
    #print Dumper $out;
    return $out;
}

1;
