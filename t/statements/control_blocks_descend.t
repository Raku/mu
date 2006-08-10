# Test the running order of BEGIN/CHECK/INIT/END
# These control blocks appear in a decending order

use v6-alpha;

use Test;

plan 13;

# L<S04/Closure traits/END "at run time" ALAP>

my $var = 1;

# vars for END
my ($evar, $ivar_at_end, $eof_var);

# vars for INIT
my ($ivar, $cvar_at_init, $evar_at_init, $var_at_init);

# vars for CHECK
my ($cvar, $bvar_at_check, $ivar_at_check, $var_at_check);

# vars for BEGIN
my ($bvar, $cvar_at_begin, $var_at_begin);

END {
    $evar++;
    $ivar_at_end = $ivar;

    # tests for END blocks:
    is $ivar_at_end, 1, '$ivar_at_end already initialized at END time' ~
        '(END {} runs at runtime, ALAP)';
    is $var, 1, '$var gets initialized at END time';
    is $eof_var, 1, '$eof_var gets assigned at END time';
}

INIT {
    $ivar++;
    $cvar_at_init = $cvar;
    $evar_at_init = $evar;
}

CHECK {
    $cvar++;
    $bvar_at_check = $bvar;
    $ivar_at_check = $ivar;
}

BEGIN {
    $bvar++;
    $cvar_at_begin = $cvar;
    $var_at_begin = $var;
}

# tests for INIT blocks:
is $ivar, 1, 'INIT {} runs only once';
is $cvar_at_init, 1, '$cvar already assigned ' ~
    '(INIT {} runs after CHECK {})';
is $evar_at_init, undef, '$evar not yet initialized at INIT time ' ~
    '(INIT {} runs before END {})';

# tests for CHECK blocks:
is $cvar, 1, 'CHECK {} runs only once';
is $bvar_at_check, 1, '$bvar already assigned ' ~
    '(CHECK {} runs after BEGIN {})';
is $ivar_at_check, undef, '$ivar not yet initialized at CHECK time ' ~
    '(CHECK {} runs before INIT {})';
is $var_at_check, undef, '$var not yet initialized at CHECK time ' ~
    '(CHECK {} runs at runtime, but ASAP)';

# tests for BEGIN blocks:
is $bvar, 1, 'BEGIN {} runs only once';
is $cvar_at_begin, undef, '$cvar not yet initialized at BEGIN time ' ~
    '(BEGIN {} runs before CHECK {})';
is $var_at_begin, undef, '$var not yet initialized at BEGIN time ' ~
    '(BEGIN {} runs at compile-time)';

$eof_var = 1;
