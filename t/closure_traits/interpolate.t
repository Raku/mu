use v6-alpha;

# Test closure traits interpolated in double-quoted strings

use Test;

plan 6;

# [TODO] add tests for ENTER/LEAVE/KEEP/UNDO/PRE/POST/etc

# L<S04/Closure traits/END "at run time" ALAP>

# IRC log:
# ----------------------------------------------------------------
# agentzh   question: should BEGIN blocks interpolated in double-quoted
#           strings be fired at compile-time or run-time?
#           for example, say "This is { BEGIN { say 'hi' } }";
# audreyt   compile time.
#           qq is not eval.

my $hist;

END {
    is $hist, 'BCIFE', 'interpolated END {...} executed';
}

is "{ END { $hist ~= 'E' } }", undef,
    'END {...} not yet executed';

is "{ FIRST { $hist ~= 'F' } }", "BCIF",
    'FIRST {...} fired at run-time, entry time of the mainline code';

is "{ INIT { $hist ~= 'I' } }", 'BCI',
    'INIT {...} fired at the beginning of runtime';

is "{ CHECK { $hist ~= 'C' } }", "BC",
    'CHECK {...} fired at compile-time, ALAP';

is "{ BEGIN { $hist ~= 'B' } }", "B",
    'BEGIN {...} fired at compile-time, ASAP';
