package PIL::PExp;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 1;

  return bless [ $_[0]->[0]->fixup ] => "PIL::PExp";
}

sub as_js { $_[0]->[0]->as_js }

1;
