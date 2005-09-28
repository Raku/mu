# Assignment.
package PIL::PAssign;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  local $_;

  die unless keys %$self == 2;
  die unless ref $self->{pLHS} eq "ARRAY";
  die unless @{ $self->{pLHS} } == 1;

  return bless {
    pLHS => $self->{pLHS}->[0]->fixup,
    pRHS => $self->{pRHS}->fixup,
  } => "PIL::PAssign";
}

sub as_js {
  my $self = shift;
  no warnings "recursion";

  return PIL::possibly_ccify $self->{pLHS}, sub {
    my $dest = shift;
    return PIL::possibly_ccify $self->{pRHS}, sub {
      my $src = shift;
      sprintf "%s(%s.STORE(%s))",
      $self->{CC}->as_js,
      $dest,
      $src;
    };
  };
}

sub unwrap { $_[0] }

1;
