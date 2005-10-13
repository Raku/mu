#!/usr/bin/pugs

# as seen on perl monks: http://www.perlmonks.org/?node_id=488867
use v6;

# The width of our square
# ($ARGV[0] or 5 if no arguments given):
my $n = @ARGS[0] // 5;      #/

# Our square, of size $n+1 x $n+1 due to the sentinel values:
my @single_line = ( 0  xx $n, -1 );
my @last_line   = ( -1 xx $n, -1 );
my @s = ( @single_line xx $n, @last_line);

# The directions we will move (right, down, left, up)
# since from $s[$p], $s[$p+1] is just to the right and
# $s[$p-$n-1] is just above:
my @d = ( 1, $n+1, -1, -$n-1 );

# Our starting direction, an index into @d; 0 for "right", $d[0]:
my $d = 0;

# Our starting position (index into @s); 0 so we start at $s[0]:
my $p = 0;

# Our starting value (to be stored into @s);
# 0 so we'll enter 1 after our first step:
my $v = 0;

# So continue while zero (not true):

for 0 .. $n*$n {
    # Store the next value where we just stepped to:
    @s[$p] = ++$v;
    # Look where we will step next.
    # If occupied (not zero, i.e. true)...
    if(  @s[ $p + @d[$d] ]  ) {
        # ...then switch to the "next" direction in @d
        # wrapping back to $d[0] if needed:
        $d = ($d+1) % @d;
    }

    #take the next step
    $p += @d[$d];
};

my $format = " %" ~ $v.chars ~ "d";
for @s { $_ .= as($format) };

for 0 .. $n - 1 -> $y {
    my $start = ($y * ($n+1));
    my $end   = $start + $n - 1;
    @s[ $start ..  $end].say;
}