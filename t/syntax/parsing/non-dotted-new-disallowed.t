use v6-alpha;

use Test;

plan 2;

# Parsing test, so should use eval to ensure it can run even if something is
# broken.

if $?PUGS_BACKEND ne "BACKEND_PUGS" {
  skip_rest "PIL2JS and PIL-Run do not support eval() yet.";
  exit;
}

# These tests are for parse-fails:
# (They check that the parser doesn't abort, but they might still parse
#  incorrectly.)
{
    class A { has $.b }

    ok( eval "new A", 'parameterless prefixed new is allowed' );

    eval_dies_ok( "new A( :b('bulbous bouffant') )", 'what looks like a constructor call is really a coersion to A, and should therefore be disallowed' );
}
