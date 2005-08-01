package PIL::PLit;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 1;
  die unless $_[0]->[0]->isa("PIL::PVal");

  return bless [ $_[0]->[0]->fixup ] => "PIL::PLit";
}

sub as_js { $_[0]->[0]->as_js }

1;
