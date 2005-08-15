package PIL::PExp;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  die unless keys %$self == 1;

  return bless { (%$self)[0] => (%$self)[1]->fixup } => "PIL::PExp";
}

sub as_js { (%{ $_[0] })[1]->as_js }

sub unwrap { (%{ $_[0] })[1] }

1;
