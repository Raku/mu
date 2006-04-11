use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Tokenizer/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use Test::More 'no_plan';

BEGIN {
    my $g = $Pugs::Grammar::Operator::operator->emit_yapp;
    print $g;

    my $p = $Pugs::Grammar::Operator::operator->emit_grammar_perl5;
    #print $p;
    eval $p;
    die "$@\n" if $@;
}

{
    my $match = Pugs::Grammar::Term->parse( q(10) );
    is_deeply( $match->(), {num=>10}, 'num' );
}

{
    my $exp = Pugs::Compiler::Rule->compile( q(
        ( %Pugs::Grammar::Term::hash     <?ws>? |
          %Pugs::Grammar::Operator::hash <?ws>? )*
    ));
    #print $rule->perl5;
    my $match = $exp->match( q(10 + $a / "abc") );
    #print Dumper( $match->[0] );
    my @m = @{$match->[0]};
    #print Dumper( @m );
    is( join(';', map { $_->() } @m), q(10 ;$a ;"abc"), 'split on terms' );
    for my $term ( @m ) {
        print $term->(), "\n";
        print Dumper ${$term}->{match}{match}[0]{match}[1]{capture};
    }
}
