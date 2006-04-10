
use Test::More tests => 2;
use Data::Dumper;

use_ok( 'Pugs::Grammar::Infix' );

{
    my $match = Pugs::Grammar::Infix->parse( q(-) );
    #print Dumper $match->();
    is_deeply( $match->(), 
        {
            'op' => 'infix:<->',
        }, 
        'infix:<->' );
}
