
use Test::More tests => 3;

use_ok( 'Pugs::Grammar::Var' );

{
    my $match = Pugs::Grammar::Var->parse( q($a) );
    is( "$match", q($a), 'var $a' );
}

{
    my $match = Pugs::Grammar::Var->parse( q(@a) );
    is( "$match", q(@a), 'var @a' );
}

