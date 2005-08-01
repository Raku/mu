package PIL::PNoop;

use warnings;
use strict;

sub as_js {
  die unless @{$_[0]} == 0;
  return "new PIL2JS.Box.Constant(undefined)";
}

1;
