package PIL::PNil;

use warnings;
use strict;

sub fixup {
  die unless keys %{ $_[0] } == 0;

  return bless {} => "PIL::PNil";
}

sub as_js {
  return "" unless $PIL::IN_SUBLIKE;

  my $cxt   = "PIL2JS.Context.ItemAny";
  my $undef = "new PIL2JS.Box.Constant(undefined)";
  my $retcc = PIL::cur_retcc;

  return "PIL2JS.generic_return($retcc).FETCH()([$cxt, $undef, 'dummycc']);";
}

1;
