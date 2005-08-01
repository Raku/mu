package PIL::PNoop;

use warnings;
use strict;

sub fixup {
  die unless @{$_[0]} == 0;

  return bless [] => "PIL::PNoop";
}

sub as_js { return "new PIL2JS.Box.Constant(undefined)" }

1;
