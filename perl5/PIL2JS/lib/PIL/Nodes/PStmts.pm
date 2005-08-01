# The lines like
#   die unless $self->[...]->isa(...)
# make sure that PIL::Parser gave us correct nodes.
package PIL::PStmts;

use warnings;
use strict;

sub as_js {
  my $self = shift;
  die unless @$self == 2;

  # Update $?POSITION.
  my $pos =
    sprintf "_24main_3a_3a_3fPOSITION.STORE(new PIL2JS.Box.Constant(%s))",
    PIL::Nodes::doublequote $CUR_POS;

  # Add a &return() to the last statement of a sub.
  if($IN_SUBLIKE and $self->[1]->isa("PIL::PNil")) {
    my $js = $self->[0]->as_js;
    # Note: Purely cosmetical hacking on the generated JS! (else it would be
    # eevil).
    $js =~ s/\n$//;
    if($IN_SUBLIKE >= PIL::Nodes::SUBROUTINE) {
      return "$pos;\n_26main_3a_3areturn.FETCH()([PIL2JS.Context.ItemAny, $js]);";
    } elsif($IN_SUBLIKE >= PIL::Nodes::SUBBLOCK) {
      return "$pos;\n_26main_3a_3aleave.FETCH()([PIL2JS.Context.ItemAny, $js]);";
    } else {
      return "$pos;\nreturn($js);";
    }
  } else {
    my @js = ($self->[0]->as_js, $self->[1]->as_js);
    $js[0] =~ s/\n$//;
    return "$pos;\n$js[0];\n$js[1]";
  }
}

1;
