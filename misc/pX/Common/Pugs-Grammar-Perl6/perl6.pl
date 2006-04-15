use lib 
    '../Pugs-Compiler-Rule/lib',
    '../Pugs-Compiler-Precedence/lib',
;

use Pugs::Grammar::Perl6;

use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

#use Test::More 'no_plan';
print q(#if 1 {10 + $a / "abc"}),"\n";
my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
if 1 {
    10 + $a / "abc"
}
1;
PERL6
use YAML;
print Dump $match->();

