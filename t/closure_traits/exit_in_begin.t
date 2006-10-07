use v6-alpha;

use Test;

# $failed is set to 0 (actually to undef) at compiletime.
my $failed;
# At run time, if we ever reach runtime, $failed is set to 1.
$failed = 1;

# When we end, we check if $failed is still 0. If yes, we've never reached runtime.
END {
  is $failed, undef,
      'exit() works in BEGIN {} - $fail not yet initialized at END time';
}

BEGIN {
  # Output the TAP header...
  plan 2;
  is $failed, undef, '$failed not yet initialized in BEGIN {}';
  # ...and exit, implicitly calling END.
  exit;
}
