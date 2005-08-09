package PIL::PStmt;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 1;

  return bless [ $_[0]->[0]->fixup ] => "PIL::PStmt";
}

sub as_js { return $_[0]->[0]->as_js . "\n" }

sub unwrap { $_[0]->[0]->unwrap }

1;
