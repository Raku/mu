#!/usr/bin/pugs

use v6;
use Test;

# $failed is set to 0 (actually to undef) at compiletime.
my $failed;
# At run time, if we ever reach runtime, $failed is set to 1.
$failed = 1;

# When we end, we check if $failed is still 0. If yes, we've never reached runtime.
END {
  is $failed // 0, 0, 'exit() works in CHECK {}';
}

CHECK {
  # Output the TAP header...
  plan 1;
  # ...and exit, implicitly calling END.
  exit;
}
