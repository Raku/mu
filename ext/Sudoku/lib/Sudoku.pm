#!/usr/bin/pugs
use v6;
class Sudoku;

has @.field;

# note that $.width and $.height are the size of sub block, not of the whole
# Sudoku. That is $!length = $.width * $.height;
has $.width;
has $.height;
has $!length;

has @!allowed;

# @!constrained contains a list of all aggregation of cells in which each
# number may only occur once. For a normal Sudoku that are rows, columns and
# blocks, for X-Sudokus that includes both main diagonals etc.
# the format is 
# ( [[x1, y1], [x2, y2], [x3, y3], ... ], 
has @!constraint;

has $!delimiter;

method init($self: Int $width, Int $height){

    PRE {
        $width > 0 and $height > 0
    }
    $.width = $width;
    $.height = $height;
    $!length = $.width * $.height;

    #initialize the pretty thing 
    @.field = map {[0 xx $!length]}, (1 .. $!length);

    @!allowed = map {[map {[1 xx $!length]}, (1 .. $!length)]}, (1 .. $!length);

    $!delimiter = join ('-' x $.width), (map {'+'}, 1 .. ($.height+1));
    $!delimiter ~= "\n";

    for (0 .. $!length - 1) -> my $i {
        #columns:
        push @!constraint, [($i xx $!length) Z (0 .. $!length - 1)];
        #rows:
        push @!constraint, [(0 .. $!length - 1) Z ($i xx $!length)];
    }

    # blocks:
    loop(my $x = 0; $x < $!length; $x += $!width){
        loop(my $y = 0; $y < $!length; $y += $!height){
            my @c;
            for (0 .. $!length-1) -> $i {
                push @c, [$x + $i % $width, $y + int($i/$width)];
            }
            push @!constraint, \@c;
        }
    }

    POST {
        $!length == $.height * $.width
    }

}

# works only for @!size <= 9
method from_string(Str $s){
#    PRE {
#        $s.elems == $!length*$!length;
#        @!size <= 9
#    }
    for (0 .. $!length*$!length-1) -> my $i {
        if +$s.substr($i, 1) {
            self.set_item(+$s.substr($i, 1), $i % $!length, int($i / $!length));
        }
    }
}

method set_item(Int $elem, Int $x, Int $y){
#    PRE {
#        0 < $elem <= $!length and
#        0 <= $x < $!length and
#        0 <= $y < $!length and
#        0 == @.field[$x][$y] and
#        1 == @!allowed[$x][$y][$elem - 1]
#    }
    @.field[$x][$y] = $elem;

    # constraint propagation
    # no more numbers are allowed where the current number is set
    for (0 .. $!length - 1) -> my $i {
        @!allowed[$x][$y][$i] = 0;
    }

    # propagation in the rest:
    for @!constraint -> my $c {
        my $in = 0;
        for @$c -> my $cell {
            if $cell[0] == $x and $cell[1] == $y {
                $in = 1;
                last;
            }
        }
        
        if $in {
            for @$c -> my $cell {
                @!allowed[$cell[0]][$cell[1]][$elem-1] = 0;
            }
        }
    }

}

method get(Int $x, Int $y){
    return @.field[$x][$y];
}

method out returns Str {
    return (map {$_.join("")}, [Z] @.field).join("");
}


method pretty ($self:)  {
    my Str $res = "";
    loop (my $i = 0; $i < @.field.elems; $i++){
        if $i % $.height == 0 {
            $res ~= $!delimiter;
        }
        # @.field is quadratic...
        loop (my $j = 0; $j < @.field.elems; $j++){
            if $j % $.width == 0 {
                $res ~= "|";
            }
            $res ~= @.field[$j][$i];
        }
        $res ~= "|\n";

    }
    $res ~= $!delimiter;
}

method pretty_print($self:) {
    print $self.pretty()
}

method is_allowed(Int $elem, Int $x, Int $y){
    PRE {
        0 < $elem <= $!length and
        0 <= $x < $!length and
        0 <= $y < $!length 
    }

    return @!allowed[$x][$y][$elem - 1];
}


method solve ($self:){
    my $i;
    while $i = $self.simple_solve2() + $self.simple_solve1() {
#        say "Debug: $i numbers found";
    }
    return $self.backtrack();
}

=head2 simple_solve1

Scans the whole puzzle for cells where only one possiblity is left

=cut

method simple_solve1($self:) {
    my $success = 0;
    for (0 .. $!length - 1) -> my $x {
        for (0 .. $!length -1) -> my $y {
            my $count = 0;
            my $pointer = 0;
            for (0 .. $!length - 1) -> my $num {
                if @!allowed[$x][$y][$num - 1] {
                    $count++;
                    $pointer = $num;
                }
            }
            if $count == 1 {
                $success++;
                $self.set_item($pointer, $x, $y);

            }

        }

    }

}

=head2 simple_solve2 

Scans a row, column and block for a position where a number can appear in only one place

=cut

method simple_solve2($self:) {

    my $success = 0;

    for @!constraint -> my $c {
        for (1 .. $!length)-> my $num {
            my $x;
            my $y;
            my $count = 0;
            for @$c -> my $tupel {
                if @!allowed[$tupel[0]][$tupel[1]][$num - 1] {
                    $count++;
                    $x = $tupel[0];
                    $y = $tupel[1];
                }
            }
            if $count == 1 {
                $self.set_item($num, $x, $y);
                $success++;
            }
        }

    }

    return $success;

}

=head2 is_solved

returns a true value if the Sudoku is fully solved

=cut

method is_solved {
    for (0 .. $!length - 1) -> my $x {
        for (0 .. $!length - 1) -> my $y {
            return 0 if @.field[$x][$y] == 0;
        }
    }
    return 1;
}

method dump {
    say "allowed: ", @!allowed.perl;
#    say "constrains: ", @!constraint.perl;
#    say "(count: ", @!constraint.elems, ")";
}

method backtrack($self:) {
#    say "backtracking...";
    for (0 .. $!length - 1) -> my $x {
        for (0 .. $!length - 1) -> my $y {
            if @.field[$x][$y] == 0 {
                # found an empty positon, let's backtrack here!
#                say "Backtracking at ($x, $y)";
                for (1 .. $!length) -> my $num {
                    if $self.is_allowed($num, $x, $y) {
#                        say "found";
                        my $copy = $self.clone;
                        $copy.set_item($num, $x, $y);
                        $copy.solve();
                        if $copy.is_solved() {
#                            say "Solved!!!111";
                            $self = $copy;
                            return 1;
                        }
                    }
                }
                return 0;
            }
        }
    }
    #obviously there was nothing to do
    return 1;
}
