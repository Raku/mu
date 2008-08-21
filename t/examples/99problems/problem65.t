use v6;
use Test;
plan 1;

# P65 (**) Layout a binary tree (2)
# 
# An alternative layout method is depicted in the illustration opposite. Find out
# the rules and write the corresponding Prolog predicate. Hint: On a given level,
# the horizontal distance between neighboring nodes is constant.
# 
# Use the same conventions as in problem P64 and test your predicate in an
# appropriate way.

# if you want to understand that alogrithm go to
# https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/ and look at the graph.
 
my $tree = ['n', 
    ['k', 
        ['c', 
            ['a', undef, undef], ['e', ['d',  undef, undef], ['g', undef, undef]], undef], ['m', undef, undef]], ['u', ['p', undef, ['q', undef, undef]], undef]];

my $expected = ['n', 15, 1,  
        ['k', 7, 2, 
            ['c', 3, 3, 
                ['a', 1, 4, undef, undef], 
                ['e', 5, 4, 
                    ['d', 4, 5, undef, undef], 
                    ['g', 6, 5, undef, undef]]], 
            ['m', 11, 3, undef, undef]], 
        ['u', 23, 2, 
            ['p', 19, 3, undef, 
                ['q', 21, 4, undef, undef]], 
            undef]];

sub height($tree){
    return 0 unless defined($tree);
    return 1 + [max] height($tree[1]), height($tree[2]);
}

my $max_height =  height($tree);

# find the x position of the root node:

my $root_x = 1;

# $base_length is the distance between the root node and its successors
my $base_length = 2 ** ($max_height - 2);
#say "base_length: $base_length";
my $p = $tree;
while defined($p[1]) {
    $root_x += $base_length;
    $base_length /= 2;
    $p = $p[1];
}
$base_length = 2 ** ($max_height - 2);

#say $tree[0], " ", $root_x;

sub align($t, $prev_x, $prev_y, $step, $lr){
    return undef unless defined($t);
#    say $t[0], " ", $step;
    if $step < 1 {
        say "weird things happen...";
        say $t.perl;
    }
    my $x;
    if $lr eq "l" {
        $x = $prev_x - $step;
    } else {
        $x = $prev_x + $step;
    }

    return [$t[0], $x, $prev_y + 1, 
           align($t[1], $x, $prev_y + 1, $step / 2, "l"),
           align($t[2], $x, $prev_y + 1, $step / 2, "r")];
}

my $result = [$tree[0], $root_x, 1, 
        align($tree[1], $root_x, 1, $base_length, "l"),
        align($tree[2], $root_x, 1, $base_length, "r")];

#say $result.perl;
#say $expected.perl;

is($result, $expected, "Layout a binary tree (2)");
