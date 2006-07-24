use v6-alpha;

use Test;

# $failed is set to 0 (actually to undef) at compiletime.
my $failed;
# At run time, if we ever reach runtime, $failed is set to 1.
$failed = 1;

# When we end, we check if $failed is still 0. If yes, we've never reached runtime.
END {
  is $failed // 0, 0, 'exit() works in BEGIN {}';
}

BEGIN {
  # Output the TAP header...
  plan 2;
  is $failed // 0, 0, 'exit() works in BEGIN {}';
  # ...and exit, implicitly calling END.
  exit;
}
