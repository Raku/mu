use v6-alpha;
use Test;
plan 93;

# P90 (**) Eight queens problem
# 
# This is a classical problem in computer science. The objective is to place
# eight queens on a chessboard so that no two queens are attacking each other;
# i.e., no two queens are in the same row, the same column, or on the same
# diagonal.
# 
# Hint: Represent the positions of the queens as a list of numbers 1..N.
# Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row
# 4, the queen in the second column is in row 2, etc. Use the generate-and-test
# paradigm.

# note that this solution tries to find _all_ solutions, thus taking its time
# if you think this is superflous just modify it to report only the first
# solution

sub collision(@a, $num = 8){
    for (0 .. $num - 1) -> $i {
        if row_collision(@a, $i) {
            say "Collision found in ", @a.join(", ");
            return 1;
        }
    }
    return 0;
}

# row_collision detects collisions only in row $row
sub row_collision(@a, $row){
    for (0 .. $row-1) -> $j {
        my $diff = @a[$row] - @a[$j];
        if $diff == 0 or $diff == $row - $j or $diff == $j - $row {
            return 1;
        }
    }
    return 0;
}

my @results;

sub search(@a, Int $row){
    if $row == 8 {
        my @b = @a;
        push @results, \@b;
    } else {
        for 0 .. 7 -> my $i {
            @a[$row] = $i;
            if 0 == row_collision(@a, $row) {
                search(@a, $row + 1);
            }
        }
    }
}

search([0 xx 8], 0);
is(@results.elems, 92, "The 8 Queens Problem has 92 solutions");
my $i = 1;
for @results -> $r {
    is(collision($r), 0, "$i-th solution is correct");
    $i++;
}
