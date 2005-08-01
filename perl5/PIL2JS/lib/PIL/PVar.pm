package PIL::PVar;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 1;
  die if     ref $_[0]->[0];

  return bless [ PIL::lookup_var $_[0]->[0] ] => "PIL::PVar";
}

sub as_js { return PIL::name_mangle $_[0]->[0] }

1;
