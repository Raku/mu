# Tests for magic variables

use v6;
require Test;

plan(2);

# Tests for %*ENV

# it must not be empty at startup
ok(0 + %*ENV.keys > 0);

# PATH is usually defined. But this test is not portable
ok(%*ENV{"PATH"} ne "");
