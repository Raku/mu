use v6-alpha;
use Test;
use Sudoku;

plan 3;

my $s = Sudoku.new();

ok $s,	"Object creation works";

$s.init(2, 2);
my $sudoku_string = 
      "1204"
    ~ "3010"
    ~ "2100" 
    ~ "0000";

my $expected =
      "1234"
    ~ "3412"
    ~ "2143"
    ~ "4321";

$s.from_string($sudoku_string);

ok !$s.is_solved, "Sudoku with 0's is not solved";

$s.solve();

is $s.out, $expected, "4x4 Sudoku solved correctly";
