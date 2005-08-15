# Binding.
package PIL::PBind;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  local $_;

  die unless keys %$self == 2;
  die unless ref $self->{pLHS} eq "ARRAY";
  die unless @{ $self->{pLHS} } == 1;

  return bless {
    pLHS => $self->{pLHS}->[0]->fixup,
    pRHS => $self->{pRHS}->fixup,
  } => "PIL::PBind";
}

sub as_js {
  my $self = shift;

  # Hack? Fully qualified variables don't need a declaration, but JavaScript
  # needs one.
  if($self->{pLHS}->isa("PIL::PVar")) {
    my $varname = $self->{pLHS}->{pVarName};
    if($varname =~ /::/) {
      $PIL::UNDECLARED_VARS{$varname}++;
    }
  }

  return PIL::possibly_ccify $self->{pRHS}, sub {
    my $src = shift;
    return PIL::possibly_ccify $self->{pLHS}, sub {
      my $dest = shift;
      sprintf "%s(%s.BINDTO(%s))",
      $self->{CC}->as_js,
      $dest,
      $src;
    };
  };
}

sub unwrap { $_[0] }

1;
