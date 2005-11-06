package PIL::Parser;
# This module takes the output of pugs -CPIL1-Perl5, eval()s it, and reblesses most
# objects (we want PIL::PVal, not PVal).

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
  die "Couldn't parse -CPIL1-Perl5 output: $@\n" if $@;
  Class::Rebless->custom($struct, "PIL", { editor => sub {
    my ($obj, $namespace) = @_;
    # We don't want PIL::Math::BigInt.
    return if ref($obj) =~ /^Math::Big/;

    bless $obj => $namespace . "::" . ref $obj;
  }});

  return $struct;
}

1;
