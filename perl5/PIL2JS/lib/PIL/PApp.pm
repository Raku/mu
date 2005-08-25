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

  my $as_js = sub {
    my @jsparams = @_; my $jsobj = pop @jsparams;

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
      use YAML; warn Dump($self);
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

  my $possibly_ccify_many = sub {
    no warnings "recursion";
    my @things = @{ +shift };
    my $sub    = shift;

    my $step; $step = sub {
      my @jsthings = @_;

      if(@things) {
        my $thisone = shift @things;
        PIL::possibly_ccify $thisone, sub {
          $step->(@jsthings, shift);
        };
      } else {
        no warnings "void";
        $self;  # XXX!!!! If this line is removed, *THINGS START TO BREAK*!!!
                # (!!!! WTF?)
        $sub->(@jsthings);
      }
    };

    ($step->(), undef $step)[0];
  };

  return $possibly_ccify_many->(
    [
      @{ $self->{pArgs} },
      $self->{pInv}
        ? $self->{pInv}
        : $self->{pFun},
    ],
    $as_js,
  );
}

sub unwrap { $_[0] }

1;
