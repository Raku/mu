use v6;
use Test;
use Sudoku;
plan 1;

# P97 (**) Sudoku
# 
# Sudoku puzzles go like this:
# 
# Problem statement                 Solution
# 
# .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7         
#         |         |                      |         |
# 6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
#         |         |                      |         |
# 5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
# --------+---------+--------      --------+---------+--------
# 3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
#         |         |                      |         |
# .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
#         |         |                      |         |
# .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
# --------+---------+--------      --------+---------+--------
# 1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
#         |         |                      |         |
# .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
#         |         |                      |         |
# 2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8
# 
# 
# Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
# column, as well as to one single 3x3 square (which we call "square" for
# short). At the beginning, some of the spots carry a single-digit number
# between 1 and 9. The problem is to fill the missing spots with digits in such
# a way that every number between 1 and 9 appears exactly once in each row, in
# each column, and in each square.

my $problem  = "004800017"
             ~ "670900000"
             ~ "508030004"
             ~ "300740100"
             ~ "069000780"
             ~ "001069005"
             ~ "100080306"
             ~ "000006091"
             ~ "240001500";

my $solution = "934825617"
             ~ "672914853"
             ~ "518637924"
             ~ "325748169"
             ~ "469153782"
             ~ "781269435"
             ~ "197582346"
             ~ "853476291"
             ~ "246391578";

my $sudoku = Sudoku.new;
$sudoku.init(3, 3);
$sudoku.from_string($problem);

# If you want to see the real solving code, just look into the module
$sudoku.solve();
is $sudoku.out, $solution, " (**) Sudoku solved";
