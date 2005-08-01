package PIL::PExp;

use warnings;
use strict;

sub as_js {
  my $self = shift;

  die unless @$self == 1;
  return $self->[0]->as_js;
}

1;
