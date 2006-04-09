
use Test::More tests => 4;

use_ok( 'Pugs::Grammar::Var' );

{
    my $match = Pugs::Grammar::Var->parse( q($a) );
    is( "$match", q($a), 'var $a' );
}

{
    my $match = Pugs::Grammar::Var->parse( q(@a) );
    is( "$match", q(@a), 'var @a' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( q(
        %Pugs::Grammar::Var::hash
    ));
    #print $rule->perl5;
    my $match = $rule->match( q($abc) );
    is( "$match", q($abc), 'hash dispatch' );
}

