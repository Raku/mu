package PIL::PNoop;

use warnings;
use strict;

sub fixup {
  die unless keys %{ $_[0] } == 0;

  return bless {} => "PIL::PNoop";
}

sub as_js {
  sprintf "(%s)(%s)",
    $_[0]->{CC}->as_js,
    "new PIL2JS.Box.Constant(undefined)";
}

sub unwrap { $_[0] }

1;
