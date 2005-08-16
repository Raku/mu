package PIL::PStmt;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  die unless keys %$self == 1;
  die unless $self->{pExpr};

  return bless { pExpr => $self->{pExpr}->fixup } => "PIL::PStmt";
}

sub as_js {
  my $self = shift;

  ($self->{pExpr}{CC} and die) or $self->{pExpr}{CC} = $self->{CC} if $self->{CC};

  return $self->{pExpr}->as_js . "\n";
}

sub unwrap { $_[0]->{pExpr} }

1;
