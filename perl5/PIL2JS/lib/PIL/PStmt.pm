package PIL::PStmt;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  die unless keys %$self == 1;

  return bless { (%$self)[0] => (%$self)[1]->fixup } => "PIL::PStmt";
}

sub as_js { return +(%{ $_[0] })[1]->as_js . "\n" }

sub unwrap { (%{ $_[0] })[1] }

1;
