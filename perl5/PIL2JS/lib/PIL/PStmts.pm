# The lines like
#   die unless $self->[...]->isa(...)
# make sure that PIL::Parser gave us correct nodes.
package PIL::PStmts;

use warnings;
use strict;

sub fixup {
  die unless keys %{ $_[0] } == 2;

  return bless {
    pStmt  =>
      ($_[0]->{pStmt}  eq "PNoop" ? bless {} => "PIL::PNoop" : $_[0]->{pStmt})->fixup,
    pStmts =>
      ($_[0]->{pStmts} eq "PNil"  ? bless {} => "PIL::PNil"  : $_[0]->{pStmts})->fixup,
  } => "PIL::PStmts";
}

sub as_js {
  my $self = shift;

  my ($head, $tail) = @$self{qw< pStmt pStmts >};
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

  return PIL::possibly_ccify $head, $cc;
}

1;
