use v6-alpha;
use Test;

plan 6;

use_ok('Automata::Cellular::Rule', 'Automata::Cellular::Rule loaded');

my Automata::Cellular::Rule $rule .= new(:rule_number<30>);

is($rule ~~ Automata::Cellular::Rule, Bool::True,  '$rule object instantiated');
is($rule.rule_number(), 30, '$rule rule_number set correctly');
is($rule.rule ~~ Hash, 1, '$rule.rule is a hash');
is(+$rule.rule<0>, 0, 'subrule 0 set up correctly');
is($rule.rule<3>, 1, 'subrule 3 set up correctly');
