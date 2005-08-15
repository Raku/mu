use warnings;
use strict;

{
  package PIL::PVal;

  sub fixup { $_[0] }

  sub as_js {
    my $self = shift;
    die unless %$self == 1;
    die unless my $v = exists $self->{pVal};
    local $_;

    return "new PIL2JS.Box.Constant(undefined)" if $v eq "VUndef";
    return sprintf "new PIL2JS.Box.Constant([%s])",
      join ", ", map { $_->as_js } @$v          if ref $v eq "ARRAY";
    return $v->as_js;
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

  sub as_js { "new PIL2JS.Box.Constant($_[0]->[0])" }
}

{
  package PIL::VRat;
  our @ISA = qw<PIL::PVal>;

  sub as_js { "new PIL2JS.Box.Constant($_[0]->[0])" }
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
