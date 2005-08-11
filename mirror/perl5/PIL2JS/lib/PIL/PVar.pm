package PIL::PVar;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 1;
  die if     ref $_[0]->[0];

  if($_[0]->[0] eq "&return") {
    PIL::fail("Can't return outside a subroutine!") unless $PIL::IN_SUBLIKE;
    # XXX hack?
    return bless [
      PIL::RawJS->new("PIL2JS.generic_return(" . PIL::cur_retcc . ")")
    ] => "PIL::PVar";
  } else {
    return bless [ PIL::lookup_var $_[0]->[0] ] => "PIL::PVar";
  }
}

sub as_js { ref $_[0]->[0] ? $_[0]->[0]->as_js : PIL::name_mangle $_[0]->[0] }

sub unwrap { $_[0] }

1;
