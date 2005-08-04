package PIL::PApp;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  local $_;

  die unless @$self == 4;
  die unless $self->[0]->isa("PIL::TCxt");
  die unless ref($self->[3]) eq "ARRAY";

  return bless [
    $self->[0]->fixup,
    $self->[1]->fixup,
    $self->[2]->isa("PIL::Just")
      ? bless [$self->[2]->[0]->fixup] => "PIL::Just"
      : $self->[2],
    [map { $_->fixup } @{ $self->[3] }],
  ] => "PIL::PApp";
}

sub as_js {
  my $self = shift;

  local $_;

  my $subname;
  if($self->[1]->[0]->isa("PIL::PVar") and not ref $self->[1]->[0]->[0]) {
    $subname = $self->[1]->[0]->[0];
  }

  my $obj;
  if(
    $self->[2]->isa("PIL::Just")           and
    $self->[2]->[0]->isa("PIL::PExp")      and
    $self->[2]->[0]->[0]->isa("PIL::PVar") and
    not ref $self->[2]->[0]->[0]->[0]      and
    1
  ) {
    $obj = $self->[2]->[0]->[0]->[0];
  }

  # XXX HACK! Support for &JS::inline.
  if(
    defined $subname                                 and
    $subname eq "&JS::inline"                        and
    $self->[3]->[0]->isa("PIL::PPos")                and
    $self->[3]->[0]->[2]->isa("PIL::PLit")           and
    $self->[3]->[0]->[2]->[0]->isa("PIL::PVal")      and
    $self->[3]->[0]->[2]->[0]->[0]->isa("PIL::VStr") and
    1
  ) {
    return $self->[3]->[0]->[2]->[0]->[0]->[0];
  } elsif(defined $subname and $subname eq "&JS::inline") {
    PIL::fail("Invalid use of &JS::inline!");
  }

  my $native;
  # true              ==> It's a call to a native JavaScript func.
  # false but defined ==> It's a call to a Perl 6 func.
  # undefined         ==> We don't know at compile-time.

  # Call to JS::foo? ==> It's a native call.
  $native = $subname =~ /^[\&\$\@\+\%\:]\*?JS::/
    if defined $subname;

  # We have an invocant? ==> We can't say for sure at compile-time.
  $native = undef
    if $self->[2]->isa("PIL::Just");

  # We have an invocant *and* the invocant is in the JS:: namespace? ==> It's
  # a native call.
  $native++ if
    defined $obj and $obj =~ /^[\&\$\@\+\%\:]\*?JS::/;
  $native++ if
    defined $obj and $subname =~ /^&\*?JS::/;

  # The sub is a reference? ==> We can't know at compile-time.
  $native = undef
    if not defined $subname or $subname =~ /^\$/;

  # Sanitize $subname.
  PIL::fail("When calling a method, the method name must be a simple string!")
    if $self->[2]->isa("PIL::Just") and (not defined $subname or $subname !~ /^&/);
  $subname = "&$1" if $self->[2]->isa("PIL::Just") and $subname =~ /^&(.+)$/;
  $subname =~ s/^(.)\*?JS::/$1/ if defined $subname;

  # Go!
  my $inv = $self->[2]->isa("PIL::Just") ? $self->[2]->[0]->as_js : "";
  my $sub = $inv || $native ? substr($subname, 1) : $self->[1]->as_js;
  my @arg = map { $_->as_js } @{ $self->[3] };
  @arg    = map { "($_).toNative()" } @arg if $native;
  my $arg = PIL::add_indent(1, join ",\n", @arg);
  my $cxt = PIL::add_indent(1, $self->[0]->as_js);

  # XXX Context handling!
  if($inv) {
    return "new PIL2JS.Box.Constant($inv.$sub(\n$arg\n))" if $native;
    #return sprintf "%s.perl_methods[%s]([\n%s,\n%s\n])",
    #  $inv, PIL::doublequote($sub), $cxt, $arg
    #  if defined $native;
    return sprintf "PIL2JS.call(%s, %s, [\n%s,\n%s\n])",
      $inv, PIL::doublequote($sub), $cxt, $arg;
  } else {
    return "new PIL2JS.Box.Constant($sub(\n$arg\n))" if $native;
    return "$sub.FETCH()([\n$cxt,\n$arg\n])"         if defined $native;
    return sprintf "PIL2JS.call(undefined, %s, [\n%s,\n%s\n])", $sub, $cxt, $arg;
  }
}

1;
