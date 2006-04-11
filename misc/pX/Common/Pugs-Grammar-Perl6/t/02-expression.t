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

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

my $match = Pugs::Grammar::Expression->parse( q(10 + $a / "abc") );
#print Dumper $match->();

is_deeply(
    $match->(),

    {
  'exp1' => {
    'num' => '10'
  },
  'exp2' => {
    'exp1' => {
      'scalar' => '$a'
    },
    'exp2' => {
      'double_quoted' => '"abc"'
    },
    'op1' => '/'
  },
      'op1' => '+'
    },

    'expression q(10 + $a / "abc")'
);
