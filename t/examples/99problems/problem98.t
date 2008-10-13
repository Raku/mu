use v6;

# P98 (***) Nonograms
# 
# Around 1994, a certain kind of puzzles was very popular in England. The
# "Sunday Telegraph" newspaper wrote: "Nonograms are puzzles from Japan and are
# currently published each week only in The Sunday Telegraph. Simply use your
# logic and skill to complete the grid and reveal a picture or diagram." As a
# Prolog programmer, you are in a better situation: you can have your computer
# do the work! Just write a little program ;-).
# 
# The puzzle goes like this: Essentially, each row and column of a rectangular
# bitmap is annotated with the respective lengths of its distinct strings of
# occupied cells. The person who solves the puzzle must complete the bitmap
# given only these lengths.
# 
# Problem statement:          Solution:
# 
# |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3           
# |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1         
# |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2         
# |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2         
# |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6           
# |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5         
# |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6           
# |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1           
# |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2           
# 1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3              
# 2 1 5 1                     2 1 5 1                      
# 
# 
# For the example above, the problem can be stated as the two lists
# [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and
# [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]] which give the "solid" lengths of
# the rows and columns, top-to-bottom and left-to-right, respectively. Published
# puzzles are larger than this example, e.g. 25 x 20, and apparently always have
# unique solutions.

###########

# (luqui) My solution will be to try every legal combination of rows,
# starting at the top and working down.  At each depth of the tree, I will
# make sure that the rows I have so far are consistent with the column
# specifications, so that there is early pruning of the search tree and
# we have a hope of finishing in a reasonable amount of time.

sub groups(@list) {
    my $on = False;
    my $group = 0;
    my @ret;
    
    for @list {
        if $_ {
            $on = True;
            $group++;
        }
        else {
            if $on {
                $on = False;
                @ret.push($group);
                $group = 0;
            }
        }       
    }

    if $on { @ret.push($group) }
    return @ret;
}

sub column_match(@col, @spec) {
    my @g = groups(@col);
    return @g == 0 || (@g[0..^@g-1] === @spec[0..^@g-1] && @g[-1] <= @spec[@g-1]);
}

sub prefix_match(@rows, @spec) {
    return True if @rows == 0;
    for 0..^@rows[0] -> $i {
        return False unless column_match(@rows.map:{ .[$i] }, @spec[$i]);
        # @rows.map:{ .[$i] }  can also be spelled @rows[*][$i]  or @rows>>.[$i],
        # neither of which work in pugs.
    }
    return True;
}

sub rowspec_solutions(@spec, $size) {
    return [0 xx $size] unless @spec;
    my $minspace = @spec - 1 + [+] @spec;
    map -> $i {
        my @prefix = 0 xx $i, 1 xx @spec[0];
        @prefix.push(0) if @spec > 1;
        map -> $sol {
            [ @prefix, @$sol ]
        }, rowspec_solutions(@spec[1..*], $size - @prefix)
    }, 0..($size - $minspace)
}

sub show_solution(@sol) {
    for @sol -> $row {
        say '# ', join '', $row.map:{ $_ ?? 'X' !! ' ' };
    }
}
    

sub find_solution(@prefix, @rowspec, @colspec) {
    return unless prefix_match(@prefix, @colspec);
    return [@prefix] if @rowspec == 0;

    for rowspec_solutions(@rowspec[0], +@colspec) -> $row {
        my @newpfx = (@prefix, $row);
        my $sol = find_solution(@newpfx, @rowspec[1..*], @colspec);
        return $sol if $sol;
    }
}

unless caller() {
    use Test;
    plan 10;

    is groups([0,1,0,0,0,1,1,0,0]), (1,2), 'groups';
    is groups([0,1,0,0,0,1,1,1,1]), (1,4), 'groups';

    ok column_match((0,1,0,0,0,1), (1,2)), 'column_match';
    ok column_match((1,1,0,1,0,1), (2,1,7,4)), 'column_match';
    ok !column_match((1,1,0,1,0,1), (2,1)), 'column_match';
    ok column_match((1,0,1,1), (1,2)), 'column_match';

    ok prefix_match(
        [[0,1,1,1,0,0,0,0],
         [1,1,0,1,0,0,0,0],
         [0,1,1,1,0,0,1,1]],
        [[1,2], [3,1], [1,5], [7,1], [5], [3], [4], [3]]), 'prefix_match';
    
    ok !prefix_match(
        [[0,1,1,1,0,0,0,0],
         [0,1,1,0,1,0,0,0],
         [0,1,1,1,0,0,1,1]],
        [[1,2], [3,1], [1,5], [7,1], [5], [3], [4], [3]]), 'prefix_match';

    is rowspec_solutions((2,1), 6),
        ((1,1,0,1,0,0),
         (1,1,0,0,1,0),
         (1,1,0,0,0,1),
         (0,1,1,0,1,0),
         (0,1,1,0,0,1),
         (0,0,1,1,0,1)), 'rowspec_solutions';

    say "# This could take a little while... (about 20 seconds on my 2.2GHz)";
    my $sol = find_solution((), ([3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]),
                                ([1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]));
    show_solution($sol);
    is $sol,
        ((0,1,1,1,0,0,0,0),
         (1,1,0,1,0,0,0,0),
         (0,1,1,1,0,0,1,1),
         (0,0,1,1,0,0,1,1),
         (0,0,1,1,1,1,1,1),
         (1,0,1,1,1,1,1,0),
         (1,1,1,1,1,1,0,0),
         (0,0,0,0,1,0,0,0),
         (0,0,0,1,1,0,0,0)), 'find_solution';
}
