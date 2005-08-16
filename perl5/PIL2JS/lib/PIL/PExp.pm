package PIL::PExp;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  die unless keys %$self == 1;
  die unless $self->{pLV};

  return bless { pLV => $self->{pLV}->fixup } => "PIL::PExp";
}

sub as_js { $_[0]->{pLV}->as_js }

sub unwrap { (%{ $_[0] })[1] }

1;
