package PIL::PNil;

use warnings;
use strict;

sub as_js {
  my $self = shift;
  die unless @$self == 0;

  return "" unless $PIL::IN_SUBLIKE;
  return "_26main_3a_3areturn.FETCH()([PIL2JS.Context.ItemAny, new PIL2JS.Box.Constant(undefined)]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
  return "_26main_3a_3aleave.FETCH()([PIL2JS.Context.ItemAny, new PIL2JS.Box.Constant(undefined)]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBBLOCK;
  return "return(new PIL2JS.Box.Constant(undefined));"
    if $PIL::IN_SUBLIKE >= PIL::SUBTHUNK;
}

1;
