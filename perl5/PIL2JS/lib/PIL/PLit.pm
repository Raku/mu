package PIL::PLit;

use warnings;
use strict;

sub as_js {
  my $self = shift;

  die unless @$self == 1;
  die unless $self->[0]->isa("PIL::PVal");
  return $self->[0]->as_js;
}

1;
