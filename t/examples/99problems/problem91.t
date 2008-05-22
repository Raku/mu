use v6;
use Test;
plan 1;

# P91 (**) Knight's tour
# 
# Another famous problem is this one: How can a knight jump on an NxN chessboard
# in such a way that it visits every square exactly once?
# 
# Hints: Represent the squares by pairs of their coordinates of the form X/Y,
# where both X and Y are integers between 1 and N. (Note that '/' is just a
# convenient functor, not division!) Define the relation jump(N,X/Y,U/V) to
# express the fact that a knight can jump from X/Y to U/V on a NxN chessboard.
# And finally, represent the solution of our problem as a list of N*N knight
# positions (the knight's tour).

# TODO reimplement this class to reuse instances as they're immutable
class Case { 
    has int $.x is ro = 0;
    has int $.y is ro = 0;
    
    method onBoard(int $n){
        0 <= $.x < $n and 0 <= $.y < $n;
    }
    
    method add(Case $c){
        Case.new( :x($.x + $c.x), :y($.y + $c.y));
    }
    
    method substract(Case $c){
        Case.new( :x($.x - $c.x), :y($.y - $c.y));
    }
    
    method equal(Case $c){
        $.x == $c.x and $.y == $c.y;
    }
    
    method toStr() {
        $.x ~ "/" ~ $.y;
    }
}

# Stringify a case
multi sub *prefix:<~> (Case $c) { $c.toStr }

# All the moves a Knight can do
our Case @possible_moves;
my $flatten = -> $x { $x.isa(Array) ?? ( map $flatten, $x ) !! $x };
for map $flatten, ((1,-1)X(2,-2)),((2,-2)X(1,-1)) -> $x,$y {
    push @possible_moves, Case.new(:x($x),:y($y));
}

sub uniq(@array) returns Bool {
    my %h = ((~<< @array) X (1));
    %h.elems == @array.elems;
}

sub valid_moves(Case @moves) returns Bool {
    my sub valid_move(Case $start, Case $end) returns Bool {
        my $move = $end.substract($start);
        [or] @possible_moves.map:{.equal($move)};
    }
    for (0..@moves.elems-2) -> $i {
        return False unless valid_move( @moves[$i], @moves[$i+1] );
    }
    return True;
}

# Check whether a tour is valid
sub check_tour(int $n, Case @moves) returns Bool {
    ([and] @moves.map:{.onBoard($n)}) # all moves are on the n*n chessboard
        and @moves.elems == $n*$n      # there is enough move to fill the whole board
        and uniq(@moves)          # all moves are different
        and valid_moves(@moves);       # each move is legal
}

# How many cases not yet visited are accessible from $case
sub count_successors(Case $case, Bool @chessboard) returns int {
    return @possible_moves.map:{.add($case)}\
        .grep:{.onBoard(@chessboard.elems) and @chessboard[.x][.y]}\
        .elems;
}

# find ONE knight's tour beginning in $first_move on a $n*$n chessboard
# finding all solutions with this kind of code is trivial (see P90) but
# extremely expensive since their numbers is really high even for small value of $n
sub searchFrom(Case $first_move, int $n) returns Array {
    my $aux = -> Case @moves, Bool @chessboard {
        if @moves.elems == $n*$n {
            return @moves;
        }
        my $last_move = @moves[-1];
        # find accessible cases where the knight didn't go already
        my @target_cases = (@possible_moves.map:{ .add($last_move) })\
            .grep:{ .onBoard($n) and @chessboard[.x][.y] };
            
        # sort them by the number of successors so as to try the less favorable first
        @target_cases = @target_cases\
            .map: -> $case {[ $case, count_successors($case, @chessboard)]}\
            .sort: {$^a[1] <=> $^b[1]}\
            .map: -> $pair {$pair[0]};
        
        # try them one by one
        for @target_cases {
            @chessboard[.x][.y] = False;
            my $solution = $aux.( [@moves,$_], @chessboard );
            return $solution if $solution;
            @chessboard[.x][.y] = True;
        }
    }
    my Bool @chessboard[$n][$n];
    for ( 0..$n-1 ) -> $i {
        @chessboard[$i] = [True xx $n];
    }
    @chessboard[$first_move.x][$first_move.y] = False;
    $aux.( [$first_move], @chessboard );
}

# this heuristic allows to find a knight's tour up to a 75x75 chessboard without backtracking
my $n = 8;
my @result = searchFrom( Case.new(:x(0),:y(0)), $n );
# @result.map:{.toStr}.join(", ").say;
ok check_tour($n, @result), "This knight's tour is valid.";
