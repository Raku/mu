package PIL::PApp;

use warnings;
use strict;

sub fixup {
  my $self = shift;
  local $_;

  die unless keys %$self == 4;
  $self->{pCxt} = bless [] => "PIL::TCxtVoid" if $self->{pCxt} eq "TCxtVoid";
  die unless $self->{pCxt}->isa("PIL::TCxt");
  die unless ref($self->{pArgs}) eq "ARRAY";

  my $subname;
  if($self->{pFun}->{pLV} and $self->{pFun}->{pLV}->isa("PIL::PVar") and not ref $self->{pFun}->{pLV}->{pVarName}) {
    $subname = $self->{pFun}->{pLV}->{pVarName};
  }
  # Minor hack -- we want syntactical pairs to be efficient.
  if($subname and $subname eq "&Pugs::Internals::named_pair") {
    my $key;
    my $unwrapped = $self->{pArgs}[0]->unwrap;
    if(
      $unwrapped->isa("PIL::PLit") and
      $unwrapped->{pLit}->isa("PIL::PVal") and
      (my $lhs = $unwrapped->{pLit}{pVal})->isa("PIL::VStr")
    ) {
      $key = $lhs->[0];
    } else {
      $key = $self->{pArgs}[0]->fixup;
    }
    my $pair = bless { pVal => bless {
      key   => $key,
      value => $self->{pArgs}->[1]->fixup,
    } => "PIL::NamedPair" } => "PIL::PLit";
    return $pair->fixup;
  }

  return bless {
    pCxt  => $self->{pCxt}->fixup,
    pFun  => $self->{pFun}->fixup,
    pInv  => $self->{pInv} && $self->{pInv}->fixup,
    pArgs => [ map { $_->fixup } @{ $self->{pArgs} } ],
  } => "PIL::PApp";
}

sub as_js {
  my $self = shift;
  local $_;
  no warnings "recursion";

  my $as_js = sub {
    my @jsparams = @_; my $jsobj = shift @jsparams;

    my $subname;
    if($self->{pFun}->{pLV} and $self->{pFun}->{pLV}->isa("PIL::PVar") and not ref $self->{pFun}->{pLV}->{pVarName}) {
      $subname = $self->{pFun}->{pLV}->{pVarName};
    }

    my $obj;
    if(
      $self->{pInv}                            and
      $self->{pInv}->isa("PIL::PExp")          and
      $self->{pInv}->{pLV}->isa("PIL::PVar")   and
      not ref $self->{pInv}->{pLV}->{pVarName} and
      1
    ) {
      $obj = $self->{pInv}->{pLV}->{pVarName};
    }

    # XXX HACK! Support for &JS::inline.
    if(
      defined $subname                                 and
      $subname eq "&JS::inline"                        and
      $self->{pArgs}->[0]->unwrap->isa("PIL::PLit")         and
      $self->{pArgs}->[0]->unwrap->{pLit}->isa("PIL::PVal")         and
      $self->{pArgs}->[0]->unwrap->{pLit}->{pVal}->isa("PIL::VStr")         and
      1
    ) {
      return sprintf "%s(\n%s\n)",
        $self->{CC}->as_js,
        PIL::add_indent 1, $self->{pArgs}->[0]->unwrap->{pLit}->{pVal}->[0];
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
      if $self->{pInv};

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
      if $self->{pInv} and (not defined $subname or $subname !~ /^&/);
    $subname = "&$1" if $self->{pInv} and $subname =~ /^&(.+)$/;
    $subname =~ s/^(.)\*?JS::/$1/ if defined $subname;

    # Go!
    my $inv = $self->{pInv} ? $jsobj : "";
    my $sub = $inv || $native ? substr($subname, 1) : $jsobj;

    # For debugging
    unless($self->{CC}) {
      warn "Internal error: Missing CC!\n";
      require YAML; warn YAML::Dump($self);
    }

    # Minor hack -- we undefine all params to not suck up all memory.
    my $cc =
      $self->{CC}->as_js(
        (grep { /^ret\d+$/ } @jsparams)
          ? join " ", map { "$_ = undefined;" } grep { /^ret\d+$/ } @jsparams
          : undef
      );

    my @arg = (@jsparams, $native ? () : ($cc));
    @arg    = map { "($_).toNative()" } @arg if $native;
    my $arg = PIL::add_indent(1, join ",\n", @arg);
    my $cxt = PIL::add_indent(1, $self->{pCxt}->as_js);

    if($subname and my $macro = $Prelude::JS::macro{$subname}) {
      local $macro->{CC} = $cc;
      return $macro->as_js(@arg);
    } elsif($inv) {
      return "$cc(new PIL2JS.Box.Constant($inv.$sub(\n$arg\n)))" if $native;
      #return sprintf "%s.perl_methods[%s]([\n%s,\n%s\n])",
      #  $inv, PIL::doublequote($sub), $cxt, $arg
      #  if defined $native;
      return sprintf "PIL2JS.call(%s, %s, [\n%s,\n%s\n])",
        $inv, PIL::doublequote($sub), $cxt, $arg;
    } else {
      return "$cc(new PIL2JS.Box.Constant($sub(\n$arg\n)))" if $native;
      return "$sub.FETCH()([\n$cxt,\n$arg\n])"              if defined $native;
      return sprintf "PIL2JS.call(undefined, %s, [\n%s,\n%s\n])", $sub, $cxt, $arg;
    }
  };

  return possibly_ccify_many(
    [
      # XXX this doesn't deal with $obj.$methref(...) (but as Pugs can't even
      # parse these constructs currently, it doesn't matter much (yet))
      $self->{pInv}
        ? $self->{pInv}
        : $self->{pFun},
      @{ $self->{pArgs} },
    ],
    $as_js,
  );
}

sub unwrap { $_[0] }

sub possibly_ccify_many {
  my ($things, $sub) = @_; 
  no warnings "recursion";

  possibly_ccify_many_step($things, 0, $sub);
}

sub possibly_ccify_many_step {
  my ($things, $i, $sub, $jsthings) = @_;
  no warnings "recursion";

  if($i <= $#{$things}) {
    my $thisone = $things->[$i++];
    PIL::possibly_ccify $thisone, sub {
      push @$jsthings, shift;
      possibly_ccify_many_step($things, $i, $sub, $jsthings);
    };
  } else {
    $sub->(@$jsthings);
  }
}

1;
