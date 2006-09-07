use v6-alpha;

##
#  Script to output Wolfram-esque cellular automata
#
#  usage: 'perl ca_wolfram.pl', followed by:
#   -r rule_number (Specify in decimal, integer from 0 to 255)
#   -s steps (Specify number of steps to display)
#   -w width
#   -i initial_state
#   -t true_char
#   -f false_char
##

use Getopt::Std;
my %opts = getopts( 'rswitf' );

##
# options, with some reasonable defaults
my $rule_number = %opts<r> || 110;
my $steps       = %opts<s> || 30;
my $width       = %opts<w> || 30;
my $initial     = %opts<i> || 'right';
my $true_char   = %opts<t> || 'x';
my $false_char  = %opts<f> || '.';

##
# Single cell in the left, right, or middle for initial state
my $left  = $steps;
my $right = $width + $steps - 1;
$width += ( $steps * 2 );
my $middle = int( $width / 2 );


##
# Initialize and fill up the array with 0
my Bool @line = 0 xx ($width-1);

##
# Set an initial state on the left, right, or middle
given $initial {
    when 'left'   { @line[$left] = 1 }
    when 'right'  { @line[$right] = 1 }
    when 'middle' { @line[$middle] = 1 }
}

##
# Unpack the Wolfram rule number into a hash

my Bool %rule_hash;
for ( 0 .. 7 ) -> $key {
    %rule_hash{$key} = ?($rule_number +& (1 ~ 0 x $key) );
}

##
# Print the rule hash, using shiny, new sprintf
say "Rule $rule_number:";
for %rule_hash.keys -> $key {
    say sprintf("%03b",$key) ~ " becomes " ~ +%rule_hash{$key};
}

##
# Render the output on the screen.
my $beginprint = $steps;
my $endprint   = @line.elems() - $steps;

while ( $steps-- ) {
    my $newline = (+<<@line[ $beginprint .. $endprint ]).join("") ;
    $newline ~~ s:g/1/$true_char/;
    $newline ~~ s:g/0/$false_char/;
    say $newline;
    
    my @old_line = @line;
    
    for ( 0 .. $width - 3 ) -> $index {
        my $index_key = :2((+<<@old_line[ $index .. $index + 2 ]).join(""));
        @line[ $index + 1 ] = %rule_hash{$index_key};
    }

}
