# Tests for magic variables

use v6;

say "1..2";

# Tests for %*ENV

# it must not be empty at startup
if (0 + %*ENV.keys > 0) { say "ok 1"; } else { say "not ok 1"; }

# PATH is usually defined. But this test is not portable
if %*ENV{"PATH"} ne "" { say "ok 2"; } else { say "not ok 2"; }
