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
        #~ { print "Grammar::Expression::parse '$_[0]' \n";
          #~ # print (keys %Pugs::Grammar::Term::hash),"\n"; 
        #~ }
        
        <?Pugs::Grammar::BaseCategory.ws>?
            #~ <Pugs::Grammar::StatementControl.parse>     <?ws>?
            #~ {
                #~ return $/{'Pugs::Grammar::StatementControl.parse'}->();
            #~ }
        #~ |
        ( 
            #{ print "trying stmt\n"; } 
            <Pugs::Grammar::StatementControl.parse> <?Pugs::Grammar::BaseCategory.ws>? 
            #{ print "stmt\n";   } 
        |
            #{ print "trying op\n"; } 
            <Pugs::Grammar::Operator.parse> <?Pugs::Grammar::BaseCategory.ws>? 
            #{ print "op\n";   } 
        |
            #{ print "trying term\n"; } 
            <Pugs::Grammar::Term.parse>     <?Pugs::Grammar::BaseCategory.ws>?
            #{ print "term\n"; } 
        )*
        #{ print "matched ", Dumper(@_); }
        #<before  \} | \{ | $ >   # XXX
        #{ print "before $_[0]\n"; }
        
        { 
            # print "Grammar::Expression::parse returning \n";
            return Pugs::Grammar::Expression::ast( $/ ); 
        }
        
        # | { print "fail $_[0]\n"; }
    !);

*parse = $parser->code;

sub ast {
    my $match = shift;

    #print "Grammar::Expression::AST $match \n";

    #print $rule->perl5;
    # my $match = $exp->match( q(10 + $a / "abc") );
    #print Dumper( $match->[0] );
    my $m = $match->[0];
    return [] unless defined $m;
    my @m = @$m;
    #print Dumper( @m );
    #is( join(';', map { $_->() } @m), q(10 ;+ ;$a ;/ ;"abc"), 'split on terms' );

    #print "Subroutines: @Pugs::Grammar::Operator::subroutine_names\n";

    my @in;
    for my $term ( @m ) {
        #print Dumper $term->();
        my ($k) = keys %$term;
        
        #print Dumper $term->{$k}();
        
        my $ast = $term->{$k}();
        if ( exists $ast->{stmt} ) {
            push @in, [ $ast->{stmt} => $ast ]
        }
        elsif ( exists $ast->{op} ) {
            push @in, [ $ast->{op} => $ast ]
        }
        elsif ( exists $ast->{bareword} ) {
            push @in, [ 'BAREWORD' => $ast ]
        }
        else {
            push @in, [ 'NUM' => $ast ]
        }
    }
    #print Dumper @in;

    my($lex) = sub {
        my($t)=shift(@in);
        $t=['',''] unless defined($t);
        print "token: $$t[0] ", Dumper( $$t[1] );
        return($$t[0],$$t[1]);
    };
    my $p = Pugs::Grammar::Operator->new(
        yylex => $lex, 
        yyerror => sub { warn "error in expression: $match ..." },
    );

    my $out=$p->YYParse;
    print Dumper $out;
    return $out;
}

1;
