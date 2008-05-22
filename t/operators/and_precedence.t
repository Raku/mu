use v6;

use Test;

plan 4;
# L<S03/Loose and precedence>
my $run = 1;

sub isfive (*@args) {
    is(@args[0], 5, "First arg is 5, run " ~ $run++)
}

isfive(5) and isfive(5);
isfive 5  and isfive 5;
