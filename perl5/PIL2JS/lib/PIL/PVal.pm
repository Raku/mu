use warnings;
use strict;

{
  package PIL::PVal;

  sub fixup { $_[0] }

  sub as_js {
    my $self = shift;
    die unless keys %$self == 1;
    die unless exists $self->{pVal};
    local $_;
    # my $v := $.pVal would be nicer... :)
    my $v = $self->{pVal};

    return "new PIL2JS.Box.Constant(undefined)" if $v eq "VUndef";
    $v = bless [[@$v]] => "PIL::VList"          if ref $v eq "ARRAY";
    return $v->as_js;
  }
}

{
  package PIL::VList;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    local $_;
    return sprintf "new PIL2JS.Box.Constant([%s])",
      join ", ", map { $_ eq "VUndef" ? "new PIL2JS.Box.Constant(undefined)" : $_->as_js } @{ $_[0]->[0] };
  }
}

# This class does not have a Haskell equivalent.
{
  package PIL::NamedPair;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $key = ref $_[0]->{key}
      ? sprintf "PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, %s]).toNative()", $_[0]->{key}->as_js
      : PIL::doublequote($_[0]->{key});
    sprintf "new PIL2JS.Box.Constant(new PIL2JS.NamedPair(%s, %s))",
      $key,
      $_[0]->{value}->as_js;
  }
}

# Basic datatypes (VInt, VRat, VStr, VUndef). Important: Box the things with
# new PIL2JS.Box.Constant!
{
  package PIL::VInt;
  our @ISA = qw<PIL::PVal>;

  sub as_js { "new PIL2JS.Box.Constant($_[0]->[0])" }
}

{
  package PIL::VNum;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    return sprintf "new PIL2JS.Box.Constant(%s)", {
      "inf"  => "Infinity",
      "-inf" => "-Infinity",
      "NaN"  => "NaN",
    }->{$self->[0]} || $self->[0];
  }
}

{
  package PIL::VRat;
  our @ISA = qw<PIL::PVal>;

  sub as_js { "new PIL2JS.Box.Constant($_[0]->[0][0]/$_[0]->[0][1])" }
}

{
  package PIL::VStr;
  our @ISA = qw<PIL::PVal>;

  sub as_js { sprintf "new PIL2JS.Box.Constant(%s)", PIL::doublequote $_[0]->[0] }
}

{
  package PIL::VBool;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    return sprintf "new PIL2JS.Box.Constant(%s)",
      $_[0]->[0] ? "true" : "false";
  }
}

1;
