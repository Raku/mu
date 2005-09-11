# The lines like
#   die unless $self->[...]->isa(...)
# make sure that PIL::Parser gave us correct nodes.
package PIL::PStmts;

use warnings;
use strict;

sub fixup {
  die unless keys %{ $_[0] } == 2;

  no warnings "recursion";
  return bless {
    pStmt  =>
      ($_[0]->{pStmt}  eq "PNoop" ? bless {} => "PIL::PNoop" : $_[0]->{pStmt})->fixup,
    pStmts =>
      ($_[0]->{pStmts} eq "PNil"  ? bless {} => "PIL::PNil"  : $_[0]->{pStmts})->fixup,
  } => "PIL::PStmts";
}

sub as_js {
  my $self = shift;
  no warnings "recursion";

  my ($head, $tail) = @$self{qw< pStmt pStmts >};
  my $cc;

  # Add a &return() to the last statement of a sub.
  if($PIL::IN_SUBLIKE and $tail->isa("PIL::PNil")) {
    $head = $head->unwrap;  # (That's also a cosmetical fix.)
    my $default = sub {
      my $cxt   = "PIL2JS.Context.ItemAny";
      my $retcc = PIL::cur_retcc;
      $cc = PIL::Cont->new(argname => "retval", body => sub {
        "PIL2JS.generic_return($retcc).FETCH()([$cxt, retval, 'dummycc']);"
      });
    };

    if($head->isa("PIL::PApp") and $head->{pFun}->unwrap->isa("PIL::PVar")) {
      my $name = $head->{pFun}->unwrap->{origName} || "";
      if($name eq "&return") {
        $cc = PIL::Cont->new(argname => "", body => sub { "" });
      } elsif($name eq "&yield") {
        $cc = PIL::RawJS->new("initial_entrypoint");
      } else {
        $default->();
      }
    } else {
      $default->();
    }
  } else {
    $cc = PIL::Cont->new(argname => "", body => $tail);
  }

  return PIL::possibly_ccify $head, $cc;
}

1;
