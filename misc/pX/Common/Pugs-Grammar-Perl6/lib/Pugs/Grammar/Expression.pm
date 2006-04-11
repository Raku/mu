package Pugs::Grammar::Expression;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

sub parse {
    my $self = shift;

    my $exp = Pugs::Compiler::Rule->compile( q(
        ( %Pugs::Grammar::Term::hash     <?ws>? |
          %Pugs::Grammar::Operator::hash <?ws>? )*
    ));
    #print $rule->perl5;
    my $match = $exp->match( q(10 + $a / "abc") );
    #print Dumper( $match->[0] );
    my @m = @{$match->[0]};
    #print Dumper( @m );
    is( join(';', map { $_->() } @m), q(10 ;+ ;$a ;/ ;"abc"), 'split on terms' );

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
            defined($t)
        or  $t=['',''];
        return($$t[0],$$t[1]);
    };
    $p=new Pugs::Grammar::Operator(yylex => $lex, yyerror => sub { die "error in expression" });

    my $out=$p->YYParse;
    print Dumper $out;
}

1;
