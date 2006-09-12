use v6-alpha;
use Automata::Cellular;
use Automata::Cellular::Rule;
use Getopt::Std;
my %opts = getopts( 'rswitf' );

##
# options, with some reasonable defaults
my Int $rule_number   = %opts<r> || 30;
my Int $steps         = %opts<s> || 30;
my Int $display_width = %opts<w> || 30;
my Str $initial       = %opts<i> || 'middle';
my Str $true          = %opts<t> || 'x';
my Str $false         = %opts<f> || '.';

##
# "actual" width is wider than display width
my Int $width = $display_width + $steps * 2;

##
# Initialize start state and fill up the array with 0
my Bool @state is rw = 0 xx $width;

##
# Set an initial state on the left, right, or middle
given $initial {
    when 'left'   { @state[$steps] = 1 }
    when 'right'  { @state[$width + $steps] = 1 }
    when 'middle' { @state[int($width/2)] = 1 }
}

##
# initialize the rule and print it
my Automata::Cellular::Rule $rule .= new(:$rule_number);
say "Rule Number $rule.rule_number()";
say $rule.pretty(:$true,:$false);

##
# initialize cellular automaton and print initial state
my Automata::Cellular $ca .=
    new( :@state, :$rule, :$steps, :$display_width);
say $ca.prettystate(:$true,:$false);

##
# Render the output on the terminal
while ( $ca++ ) {
    say $ca.prettystate(:$true,:$false);
}
