
use Test::More tests => 4;
use Data::Dumper;

use_ok( 'Pugs::Grammar::Var' );

{
    my $match = Pugs::Grammar::Var->parse( q($a) );
    is( "$match", q($a), 'var $a' );
    #print Dumper $match->();
    is_deeply( $match->(), 
        {
            'scalar' => '$a',
        }, 
        'var $a' );
}

{
    my $match = Pugs::Grammar::Var->parse( q(@a) );
    is( "$match", q(@a), 'var @a' );
}

