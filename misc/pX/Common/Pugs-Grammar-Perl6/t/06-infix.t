use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Tokenizer/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Test::More tests => 2;
use Data::Dumper;

use Pugs::Grammar::Prefix;
use_ok( 'Pugs::Grammar::Infix' );

{
    my $match = Pugs::Grammar::Infix->parse( q(-) );
    #print Dumper $match->();
    is_deeply( $match->(), 
        {
            'op' => '-',
        }, 
        'infix:<->' );
}
