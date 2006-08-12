use v6-alpha;

# Test the running order of BEGIN/CHECK/INIT/END
# These blocks appear in ascending order
# [TODO] add tests for ENTER/LEAVE/KEEP/UNDO/PRE/POST/etc

use Test;

plan 7;

# L<S04/Closure traits/END "at run time" ALAP>

my $var;
my ($var_at_begin, $var_at_check, $var_at_init, $var_at_first);
my $eof_var;

$var = 13;

my $hist;

BEGIN {
    $hist ~= 'begin ';
    $var_at_begin = $var;
}

CHECK {
    $hist ~= 'check ';
    $var_at_check = $var;
}

INIT {
    $hist ~= 'init ';
    $var_at_init = $var;
}

FIRST {
    $hist ~= 'first ';
    $var_at_first = $var;
}

END {
    # tests for END blocks:
    is $var, 13, '$var gets initialized at END time';
    is $eof_var, 29, '$eof_var gets assigned at END time';
}

is $hist, 'begin check init first ', 'BEGIN {} runs only once';
is $var_at_begin, undef, 'BEGIN {...} ran at compile time';
is $var_at_check, undef, 'CHECK {...} ran at compile time';
is $var_at_init, undef, 'INIT {...} ran at runtime, but ASAP';
is $var_at_first, undef, 'FIRST {...} at runtime, but before the mainline body';

$eof_var = 29;
