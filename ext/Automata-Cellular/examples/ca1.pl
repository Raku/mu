use v6-alpha;
use Automata::Cellular;
use Automata::Cellular::Rule;

# Perl 6 script, utilizing Perl 6 Automata::Cellular modules to render CA on
# a terminal, using characters of choice.  Occasionally known to segfault
# after 5 lines of output, due to a  string.c bug in Parrot 0.4.6.  This bug
# should be absent from the subsequent release of Parrot.

##
# options, with some reasonable defaults
use Getopt::Std;
my %opts = getopts( 'rswitf' );
my Int $rule_number   = %opts<r> || 110;
my Int $steps         = %opts<s> || 30;
my Int $display_width = %opts<w> || 30;
my Str $initial       = %opts<i> || 'middle';
my Str $true          = %opts<t> || 'x';
my Str $false         = %opts<f> || '.';

##
# "actual" width is wider than display width
my Int $width = $display_width + $steps * 2;

##
# Initialize start state and fill up the array with 0, then set either the
# left, right, or middle bit to 1
my Bool @state is rw = 0 xx $width;
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

1;
