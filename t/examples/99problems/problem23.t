use v6;
use Test;
plan 5;

# P23 (**) Extract a given number of randomly selected elements from a list.
# 
# The selected items shall be returned in a list.
# Example:
# * (rnd-select '(a b c d e f g h) 3)
# (E D A)
# 
# Hint: Use the built-in random number generator and the result of problem P20.

sub rand_select(int $count, *@list) returns Array {
    # from Larry (on #perl6):  well, pick(3) is specced to do that but is unimplemented.
    return map { @list[rand(@list.elems).floor] }, 1 .. $count;
}
my @letters = 'a' .. 'h';
my @rand = rand_select(3, @letters);
is @rand.elems, 3, 'rand_select() should return the correct number of items';

# of course the following is wrong, but it also confuses test output!
#ok all(@rand) ~~ none(@letters), '... and they should be in the letters';
ok all(@rand) ~~ any(@letters), '... and they should be in the letters';

@rand = <a b c d e f g h>.pick(3);
is @rand.elems, 3, 'pick() should return the correct number of items';
ok all(@rand) ~~ any(@letters), '... and they should be in the letters';

my $compress = sub ($x) {
    state $previous;
    return $x ne $previous ?? ($previous = $x) !! ();
}
@rand = map $compress, @rand;
is @rand.elems, 3, '... and pick() should return unique elements';
