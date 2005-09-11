package PIL::PExp;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  die unless keys %$self == 1;
  die unless $self->{pLV};

  return bless { pLV => $self->{pLV}->fixup } => "PIL::PExp";
}

sub as_js {
  my $self = shift;

  ($self->{pLV}{CC} and die) or $self->{pLV}{CC} = $self->{CC} if $self->{CC};

  no warnings "recursion";
  return $self->{pLV}->as_js;
}

sub unwrap { $_[0]->{pLV} }

1;
