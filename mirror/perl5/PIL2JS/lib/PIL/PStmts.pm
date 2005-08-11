# The lines like
#   die unless $self->[...]->isa(...)
# make sure that PIL::Parser gave us correct nodes.
package PIL::PStmts;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 2;

  return bless [
    $_[0]->[0]->fixup,
    $_[0]->[1]->fixup,
  ] => "PIL::PStmts";
}

sub as_js {
  my $self = shift;

  # Update $?POSITION.
  my $pos =
    sprintf "_24main_3a_3a_3fPOSITION.STORE(new PIL2JS.Box.Constant(%s))",
    PIL::doublequote $PIL::CUR_POS;

  my ($head, $tail) = @$self;
  my $cc;
  if($PIL::IN_SUBLIKE and $tail->isa("PIL::PNil")) {
    # Add a &return() to the last statement of a sub.
    $head   = $head->unwrap;  # (That's only a cosmetical fix.)
    my $cxt   = "PIL2JS.Context.ItemAny";
    my $retcc = PIL::cur_retcc;
    $cc = PIL::Cont->new(argname => "retval", body => sub {
      "PIL2JS.generic_return($retcc).FETCH()([$cxt, retval, 'dummycc']);"
    });
  } else {
    $cc = PIL::Cont->new(argname => "", body => $tail);
  }

  return $pos . ";\n" . PIL::possibly_ccify $head, $cc;
}

1;
