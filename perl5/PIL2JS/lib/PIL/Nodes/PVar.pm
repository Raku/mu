package PIL::PVar;

use warnings;
use strict;

sub as_js {
  my $self = shift;

  die unless @$self == 1;
  die if     ref $self->[0];

  return PIL::Nodes::name_mangle($self->[0]);
  # return "__pil2js_lookup(" . PIL::Nodes::doublequote($self->[0]) . ")";
}

1;
