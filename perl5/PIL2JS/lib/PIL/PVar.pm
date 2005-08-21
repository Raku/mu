package PIL::PVar;

use warnings;
use strict;

sub fixup {
  die unless keys %{ $_[0] } == 1;
  die if     ref(my $name = $_[0]->{pVarName});
  local $_;

  # XXX hacks?
  if($name eq "&return") {
    PIL::fail("Can't return outside a subroutine!")
      unless grep { $_ >= PIL::SUBROUTINE } @PIL::IN_SUBLIKES;
    return bless {
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(subreturncc)")
    } => "PIL::PVar";
  } elsif($name eq "&?CALLER_CONTINUATION") {
    PIL::fail("There's no &?CALLER_CONTINUATION outside a subroutine!")
      unless grep { $_ >= PIL::SUBROUTINE } @PIL::IN_SUBLIKES;
    return bless {
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(subreturncc)")
    } => "PIL::PVar";
  } elsif($name eq "&yield") {
    PIL::fail("Can't yield outside a coroutine!")
      unless grep { $_ == PIL::SUBCOROUTINE } @PIL::IN_SUBLIKES;
    return bless {
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(coroyieldcc)")
    } => "PIL::PVar";
  } else {
    return bless { pVarName => PIL::lookup_var $_[0]->{pVarName} } => "PIL::PVar";
  }
}

sub as_js { ref $_[0]->{pVarName} ? $_[0]->{pVarName}->as_js : PIL::name_mangle $_[0]->{pVarName} }

sub unwrap { $_[0] }

1;
