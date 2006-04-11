use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Tokenizer/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Test::More 'no_plan';

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Pugs::Grammar::Expression;
use Pugs::Grammar::StatementControl;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

{
    my $match = Pugs::Grammar::Expression->parse( q(10) );
    #print Dumper $match->();
    is_deeply(
        $match->(), { 'num' => '10' ,}, 
        'the expression compiler looks ok' );
}

my $match = Pugs::Grammar::StatementControl->parse( q({10}) );
#print Dumper $match->();

is_deeply(
    $match->(),

    {
      'bare_block' => {
        'num' => '10'
      }
    },

    'bare block'
);
