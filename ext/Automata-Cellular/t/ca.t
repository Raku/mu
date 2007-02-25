use v6-alpha;
use Test;

plan 6;

#use_ok('Automata::Cellular::Rule', 'Automata::Cellular::Rule loaded');
#use_ok('Automata::Cellular', 'Automata::Cellular loaded');

use Automata::Cellular::Rule; pass'Automata::Cellular::Rule loaded');
use Automata::Cellular; pass('Automata::Cellular loaded');

{
    my Automata::Cellular::Rule $rule .= new(:rule_number<30>);
    my @state = (0,0,0,0,1,0,0,0,0);
    my $steps = 3;
    my $display_width = 3;

    my Automata::Cellular $ca .= new(:@state, :$rule, :$steps, :$display_width);

    is($ca ~~ ::Automata::Cellular, Bool::True, '$ca object instantiated');

    is($ca.stage,  1,                   'instantiated with stage 0');
    is($ca.state,  '0 0 0 0 1 0 0 0 0', 'correct initial state');
    
    $ca++;

    is($ca.stage,  2,                   'stage increments correctly');
}
