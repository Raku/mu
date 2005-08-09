# Binding.
package PIL::PBind;

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
  ] => "PIL::PBind";
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

  return PIL::possibly_ccify $self->[1], sub {
    my $src = shift;
    return PIL::possibly_ccify $self->[0]->[0], sub {
      my $dest = shift;
      sprintf "%s(%s.BINDTO(%s))",
      $self->[PIL::CC]->as_js,
      $dest,
      $src;
    };
  };
}

sub unwrap { $_[0] }

1;
