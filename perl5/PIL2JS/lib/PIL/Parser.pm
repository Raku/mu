package PIL::Parser;
# This module parses PIL (as given by pugs -CPIL) into a structure of objects.
# Its design is slightly influenced by Haskell's excellent Parsec module.

use warnings;
use strict;

use Class::Rebless;

# Main class method: Parses the given PIL.
sub parse {
  my ($class, $str) = @_;

  die "No string to parse given!\n" unless defined $str;

  my $struct = eval $str;
  Class::Rebless->rebase($struct, "PIL");

  return $struct;
}

1;
