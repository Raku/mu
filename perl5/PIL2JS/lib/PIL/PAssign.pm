# Assignment.
package PIL::PAssign;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  local $_;

  die unless @$self == 2;
  die unless ref $self->[0] eq "ARRAY";
  die unless @{ $self->[0] } == 1;

  return bless [
    [map { $_->fixup } @{ $self->[0] }],
    $self->[1]->fixup,
  ] => "PIL::PAssign";
}

sub as_js {
  my $self = shift;

  # Hack? Fully qualified variables don't need a declaration, but JavaScript
  # needs one.
  if($self->[0]->[0]->isa("PIL::PVar")) {
    my $varname = $self->[0]->[0]->[0];
    if($varname =~ /::/) {
      $PIL::UNDECLARED_VARS{$varname}++;
    }
  }

  return sprintf "%s.STORE(%s)",
    $self->[0]->[0]->as_js,
    $self->[1]->as_js;
}

1;
