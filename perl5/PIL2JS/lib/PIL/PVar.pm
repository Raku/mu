package PIL::PVar;

use warnings;
use strict;

sub fixup {
  die unless keys %{ $_[0] } == 1;
  die if     ref(my $name = $_[0]->{pVarName});

  if($name eq "&return") {
    PIL::fail("Can't return outside a subroutine!")
      unless $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
    # XXX hack?
    return bless {
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(subreturncc)")
    } => "PIL::PVar";
  } else {
    return bless { pVarName => PIL::lookup_var $_[0]->[0] } => "PIL::PVar";
  }
}

sub as_js { ref $_[0]->{pVarName} ? $_[0]->{pVarName}->as_js : PIL::name_mangle $_[0]->{pVarName} }

sub unwrap { $_[0] }

1;
