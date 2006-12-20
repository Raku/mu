use v6-alpha;
use Test;
plan 1;

# P97 (**) Sudoku
# 
# Sudoku puzzles go like this:
# 
# Problem statement                 Solution
# 
# .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7         
# |         |                      |         |
# 6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
# |         |                      |         |
# 5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
# --------+---------+--------      --------+---------+--------
# 3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
# |         |                      |         |
# .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
# |         |                      |         |
# .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
# --------+---------+--------      --------+---------+--------
# 1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
# |         |                      |         |
# .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
# |         |                      |         |
# 2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8
# 
# 
# Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
# column, as well as to one single 3x3 square (which we call "square" for
# short). At the beginning, some of the spots carry a single-digit number
# between 1 and 9. The problem is to fill the missing spots with digits in such
# a way that every number between 1 and 9 appears exactly once in each row, in
# each column, and in each square.

if 1 {
    skip 1, "Test(s) not yet written: (**) Sudoku";
}
else {
    ok 1, '(**) Sudoku';
}
