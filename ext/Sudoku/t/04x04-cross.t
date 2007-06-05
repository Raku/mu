use v6-alpha;
use Test;
use Sudoku;

plan 1;

skip("X sudokus don't work yet");
exit 0;

my $problem  = "1204"
             ~ "0400"
             ~ "0300"
             ~ "0000";

my $solution = "1234"
             ~ "3412"
             ~ "4321"
             ~ "2143";

my Sudoku $s = Sudoku.new();
$s.init(2, 2);
$s.from_string($problem);

$s.cross;

$s.solve;
#$s.pretty_print;

is $s.out, $solution, "X Sudoku solved";

# vim: syn=perl6:sw=4:tw=4:expandtab

