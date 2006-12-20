use v6-alpha;
use Test;
plan 1;

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

if 1 {
    skip 1, "Test(s) not yet written: (***) Nonograms";
}
else {
    ok 1, '(***) Nonograms';
}
