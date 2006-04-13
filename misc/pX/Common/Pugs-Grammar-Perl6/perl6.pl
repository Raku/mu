use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Pugs::Compiler::Rule;
use Pugs::Grammar::Precedence;
use Pugs::Grammar::Term;
use Pugs::Grammar::Operator;

use Pugs::Grammar::Expression;
use Pugs::Grammar::StatementControl;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

use Test::More 'no_plan';
print q(#if 1 {10 + $a / "abc"}),"\n";
my $match = Pugs::Grammar::StatementControl->statement_list(<<'PERL6');
if 1 {10 + $a / "abc"}
PERL6
use YAML;
print Dump $match->();

