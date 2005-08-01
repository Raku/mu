package PIL::PNil;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 0;

  return bless [] => "PIL::PNil";
}

sub as_js {
  return "" unless $PIL::IN_SUBLIKE;

  my $cxt = "PIL2JS.Context.ItemAny";
  return "_26main_3a_3areturn.FETCH()([$cxt, new PIL2JS.Box.Constant(undefined)]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
  return "_26main_3a_3aleave.FETCH()([$cxt, new PIL2JS.Box.Constant(undefined)]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBBLOCK;
  return "return(new PIL2JS.Box.Constant(undefined));"
    if $PIL::IN_SUBLIKE >= PIL::SUBTHUNK;
}

1;
