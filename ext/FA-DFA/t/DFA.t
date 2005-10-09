use Test;
use FA::DFA;

plan 4;

my FA::DFA $dfa .= new;
ok($dfa,"Object created");

my $startstate = $dfa.state;
my FA::DFA::Node $nextnode = $dfa.addarc("test");

ok($nextnode,"Node created");

$dfa.next("test")
is($dfa.state,$nextnode,"Nodes can be stepped threw");

$dfa.reset;
is($dfa.state,$startstate,"Nodes can be reset");

my FA::DFA::Node $nextnode2 = $dfa.addarc("test2",$nextnode);
is($nextnode,$nextnode2,"Arcs to defined nodes will be created");






