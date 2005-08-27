package PIL::Parser;
# This module parses PIL (as given by pugs -CPIL) into a structure of objects.
# Its design is slightly influenced by Haskell's excellent Parsec module.

use warnings;
use strict;

# To s/VNum/PIL::VNum/g
use Class::Rebless;
# To support Inf, -Inf, and NaN
use Math::BigInt;

# Main class method: Parses the given PIL.
sub parse {
  my ($class, $str) = @_;

  die "No string to parse given!\n" unless defined $str;

  local $@;

  my $struct = eval $str;
  die "Couldn't parse -CPerl5 output: $@\n" if $@;
  Class::Rebless->custom($struct, "PIL", { editor => sub {
    my ($obj, $namespace) = @_;
    # We don't want PIL::Math::BigInt.
    return if ref($obj) =~ /^Math::Big/;

    bless $obj => $namespace . "::" . ref $obj;
  }});

  return $struct;
}

1;
