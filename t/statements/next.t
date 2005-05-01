#!/usr/bin/pugs

use v6;
use Test;

=kwid

next
next if <condition>;
<condition> and next;
next <label>;
next in nested loops
next <label> in nested loops

=cut

plan 7;

# test for loops with next

{
    eval_is(
        'my $tracker=0;for (1..2) { next; $tracker++;} $tracker',
        0,
        "tracker is 0 because next before increment",
    );
}

{
    eval_is(
        'my $tracker = 0; for (1..5) { next if 2 < $_ < 4; $tracker = $_;} $tracker',
        3,
        "... nothing before or after 3 (next if <cond>)",
        :todo(1)
    );
}

{
    eval_is(
        'my $tracker = 0; for (1..5) { $_ > 3 && next; $tracker = $_;} $tracker',
        3,
        "... nothing after 3 (<cond> && next)",
    );
}

{
    eval_is(
        'my $tracker = 0; for (1..5) { $_ > 3 and next; $tracker = $_;} $tracker',
        3,
        "... nothing after 3 (<cond> and next)",
    );
}

{
    eval_is(
        'my $tracker=0; DONE: for (1..2) { next DONE; $tracker++;} $tracker',
        0,
        "tracker is 0 because next before increment",
        :todo(1)
    );
}

{
    eval_is('my $tracker=0;for (1..5)->$out {for (10..11)->$in {next if $out > 2;$tracker = $in + $out;}}$tracker;',
        13,
        'inner loop skips once inner is run twice (next inside nested loops)',
    );
}

{
    eval_is(
        'my $tracker=0; OUT: for (1..2) { IN: for (1..2) { next OUT; $tracker++; } } $tracker',
        0,
        "tracker is 0 because next before increment in nested loop",
        :todo(1)
    );
}
