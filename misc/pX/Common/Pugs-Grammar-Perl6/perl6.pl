use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Pugs::Grammar::Expression;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use Test::More 'no_plan';

my $match = Pugs::Grammar::Expression->parse( q(10 + $a / "abc") );
print Dumper $match->();

