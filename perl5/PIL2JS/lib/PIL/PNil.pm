package PIL::PNil;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 0;

  return bless [] => "PIL::PNil";
}

sub as_js {
  return "" unless $PIL::IN_SUBLIKE;

  my $cxt   = "PIL2JS.Context.ItemAny";
  my $undef = "new PIL2JS.Box.Constant(undefined)";
  return "_26main_3a_3areturn.FETCH()([$cxt, $undef]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
  return "_26main_3a_3aleave.FETCH()([$cxt, $undef]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBBLOCK;
  # !!! *Never* do a native JS return(), as it defeats var restore.
  return "_26PIL2JS_3a_3aInternals_3a_3asmallreturn.FETCH()([$cxt, $undef]);"
    if $PIL::IN_SUBLIKE >= PIL::SUBTHUNK;
}

1;
