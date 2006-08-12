use v6-alpha;

# Test the running order of closure traits
# These blocks appear in descending order
# [TODO] add tests for ENTER/LEAVE/KEEP/UNDO/PRE/POST/etc

use Test;

plan 7;

# L<S04/Closure traits/END "at run time" ALAP>

my $var;
my ($var_at_first, $var_at_init, $var_at_check, $var_at_begin);
my $eof_var;

$var = 13;

my $hist;

END {
    # tests for END blocks:
    is $var, 13, '$var gets initialized at END time';
    is $eof_var, 29, '$eof_var gets assigned at END time';
}

FIRST {
    $hist ~= 'first ';
    $var_at_first = $var;
}

INIT {
    $hist ~= 'init ';
    $var_at_init = $var;
}

CHECK {
    $hist ~= 'check ';
    $var_at_check = $var;
}

BEGIN {
    $hist ~= 'begin ';
    $var_at_begin = $var;
}

is $hist, 'begin check init first ', 'BEGIN {} runs only once';
is $var_at_begin, undef, 'BEGIN {...} ran at compile time';
is $var_at_check, undef, 'CHECK {...} ran at compile time';
is $var_at_init, undef, 'INIT {...} ran at runtime, but ASAP';
is $var_at_first, undef, 'FIRST {...} at runtime, but before the mainline body';

$eof_var = 29;
