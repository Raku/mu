package PIL::PLit;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  die unless keys %$self == 1;
  die unless $_[0]->{pVal}->isa("PIL::PVal");

  return bless { pVal => $_[0]->{pVal}->fixup } => "PIL::PLit";
}

sub as_js { $_[0]->{pVal}->as_js }

sub unwrap { $_[0] }

1;
