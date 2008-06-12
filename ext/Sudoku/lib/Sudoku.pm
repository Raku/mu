#!/usr/bin/pugs
use v6;

=begin pod

=head1 NAME

Sudoku - a simple Sudoku solver

=head1 SYNOPSIS

    my Sudoku $s = Sudoku.new;
    # initialize with a 3x2 block size, overall size will be 6x6
    $s.init(3, 2);

    # it's easiest to initialize by a string:
    my $start = "041300"
              ~ "005106"
              ~ "000502"
              ~ "300460"
              ~ "020003"
              ~ "060050";
    $s.from_string($start);

    # solve that Sudoku:
    $s.solve;

    # simple ASCII-Art output
    $s.pretty_print;

    # and in one line:
    say $s.out;

=head1 DESCRIPTION

=head1 METHODS

=end pod

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

=begin pod

=head3 method C<init($self: Int $width, Int $height);>

sets the dimensions of a Sudoku block to C<$width> x C<$height>, the 
overall length will be the product of these two values.

You have to call this method before you cann add any clues.

=end pod

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

    for (0 .. $!length - 1) >  $i {
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

=begin pod

=head3 C<method from_string(Str $s);>
    
initialize the Sudoku from a string C<$s>, with a 0 denoting an empty cell
and a number between 1 and 9 a clue.

Note that there is currently no way to use this function for sizes bigger 
than 9x9 overall length.

=end pod

# works only for @!size <= 9
method from_string(Str $s){
#    PRE {
#        $s.chars == $!length*$!length;
#    }
    for (0 .. $!length*$!length-1) -> $i {
        if +$s.substr($i, 1) {
            self.set_item(+$s.substr($i, 1), $i % $!length, int($i / $!length));
        }
    }
}

=begin pod

=head3 C<method set_item(Int $elem, Int $x, Int $y);>

add a clue with value C<$elem> at the coordinates (C<$x>, C<$y>).

It will possibly raise an error if it violates a Sudoku rule to that
clue.

=end pod

method set_item($self: Int $elem, Int $x, Int $y){
#    PRE {
#        0 < $elem <= $!length and
#        0 <= $x < $!length and
#        0 <= $y < $!length and
#        0 == @.field[$x][$y] and
#        1 == @!allowed[$x][$y][$elem - 1]
#    }
    die "Not allowed\n" unless $self.is_allowed($elem, $x, $y);
    @.field[$x][$y] = $elem;

    # constraint propagation
    # no more numbers are allowed where the current number is set
    for (0 .. $!length - 1) -> $i {
        @!allowed[$x][$y][$i] = 0;
    }

    # propagation in the rest:
    for @!constraint -> $c {
        my $in = 0;
        for @$c -> $cell {
            if $cell[0] == $x and $cell[1] == $y {
                $in = 1;
                last;
            }
        }
        
        if $in {
            for @$c -> $cell {
                @!allowed[$cell[0]][$cell[1]][$elem-1] = 0;
            }
        }
    }

}

=begin pod

=head3 method get(Int $x, Int $y);

returns the number at the given positions or C<0> if there is none yet.

=end pod

method get(Int $x, Int $y){
    return @.field[$x][$y];
}

=begin pod

=head3 method out returns Str;

returns a String representation of the Sudoku like the one accepted by 
C<from_string>

=end pod

method out returns Str {
    # [Z] transposes the matrix in @.field
    return (map {$_.join("")}, [Z] @.field).join("");
}

=begin pod 

=head3 method pretty ($self:) returns Str;

Returns an ASCII art representation of the Sudoku

=end pod


method pretty ($self:) returns Str {
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

=begin pod 

=head3 method pretty_print($self:)

prints an ASCII art representation of the Sudoku to standard output

=end pod

method pretty_print($self:) {
    print $self.pretty()
}

method is_allowed(Int $elem, Int $x, Int $y){
#    PRE {
#        0 < $elem <= $!length and
#        0 <= $x < $!length and
#        0 <= $y < $!length 
#    }

    return @!allowed[$x][$y][$elem - 1];
}


method solve ($self:){
    my $i;
    while $i = $self.simple_solve2() + $self.simple_solve1() {
#        say "Debug: $i numbers found";
    }
    return $self.backtrack();
}

method cross {
#   @!constraint.push:  slice ^$!length Z ^$!length ;
#   @!constraint.push:  slice (^$!length).reverse Z ^$!length ;
    my @r;
    my @l;
    for ^$!length {
        @l.push: [$_, $_];
        @r.push: [$_, $!length - $_ - 1];
    }
    @!constraint.push: \@l, \@r;
#    for @!constraint -> $c {
#        say $c.perl;
#    }
}

# Scans the whole puzzle for cells where only one possiblity is left

method simple_solve1($self:) {
    my $success = 0;
    for (0 .. $!length - 1) -> $x {
        for (0 .. $!length -1) -> $y {
            my $count = 0;
            my $pointer = 0;
            for (0 .. $!length - 1) -> $num {
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


#Scans a row, column and block for a position where a number can appear in only one place


method simple_solve2($self:) {

    my $success = 0;

    for @!constraint -> $c {
        for (1 .. $!length)-> $num {
            my $x;
            my $y;
            my $count = 0;
            for @$c -> $tupel {
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

#returns a true value if the Sudoku is fully solved

method is_solved {
    for (0 .. $!length - 1) -> $x {
        for (0 .. $!length - 1) -> $y {
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
    for (0 .. $!length - 1) -> $x {
        for (0 .. $!length - 1) -> $y {
            if @.field[$x][$y] == 0 {
                # found an empty positon, let's backtrack here!
#                say "Backtracking at ($x, $y)";
                for (1 .. $!length) -> $num {
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

# vim:syn=perl6:sw=4:ts=4:expandtab
