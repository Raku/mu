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
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(subreturncc)"),
      origName => "&return",
    } => "PIL::PVar";
  } elsif($name eq "&?CALLER_CONTINUATION") {
    PIL::fail("There's no &?CALLER_CONTINUATION outside a subroutine!")
      unless grep { $_ >= PIL::SUBROUTINE } @PIL::IN_SUBLIKES;
    return bless {
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(subreturncc)"),
      origName => "&?CALLER_CONTINUATION",
    } => "PIL::PVar";
  } elsif($name eq "&yield") {
    PIL::fail("Can't yield outside a coroutine!")
      unless grep { $_ == PIL::SUBCOROUTINE } @PIL::IN_SUBLIKES;
    return bless {
      pVarName => PIL::RawJS->new("PIL2JS.generic_return(coroyieldcc)"),
      origName => "&yield",
    } => "PIL::PVar";
  } else {
    return bless {
      pVarName => PIL::lookup_var $_[0]->{pVarName},
      origName => $_[0]->{pVarName},
    } => "PIL::PVar";
  }
}

sub as_js {
  my $self = shift;

  if(ref $self->{pVarName}) {
    ($self->{pVarName}{CC} and die) or $self->{pVarName}{CC} = $self->{CC} if $self->{CC};
    return $self->{pVarName}->as_js;
  } else {
    # Hack? Fully qualified variables don't need a declaration, but JavaScript
    # needs one.
    my $name = $self->{pVarName};
    if($name =~ /::/ and $name !~ /CALLER::/) {
      $PIL::UNDECLARED_VARS{$name}++;
    }

    my $jsvar = PIL::name_mangle $name;
    if($self->{CC}) {
      return $self->{CC}->as_js . "($jsvar)";
    } else {
      return $jsvar;
    }
  }
}

sub unwrap { $_[0] }

1;
