use warnings;
use strict;

{
  package PIL::PVal;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die unless $self->[0]->isa("PIL::PVal");
    return $self->[0]->as_js;
  }
}

# Basic datatypes (VInt, VRat, VStr, VUndef). Important: Box the things with
# __pil2js_box!
{
  package PIL::VInt;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];
    return "new PIL2JS.Box.Constant($self->[0])";
  }
}

{
  package PIL::VNum;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die unless ref $self->[0];
    return $self->[0]->as_js;   # XXX?
  }
}

{
  package PIL::VRat;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];
    return sprintf "new PIL2JS.Box.Constant(%s)", join "/", split "%", $self->[0];
  }
}

{
  package PIL::VStr;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 1;
    die if     ref $self->[0];
    return sprintf "new PIL2JS.Box.Constant(%s)", PIL::Nodes::doublequote $self->[0];
  }
}

{
  package PIL::VBool;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;
    die unless @$self == 1;

    return sprintf "new PIL2JS.Box.Constant(%s)",
      $self->[0]->isa("PIL::True")  ? "true"  :
      $self->[0]->isa("PIL::False") ? "false" : die;
  }
}

{
  package PIL::VList;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;
    die unless @$self == 1;

    local $_;
    return sprintf "new PIL2JS.Box.Constant([%s])",
      join ", ", map { $_->as_js } @{ $self->[0] };
  }
}

{
  package PIL::VUndef;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 0;
    return "new PIL2JS.Box.Constant(undefined)";
  }
}

{
  package PIL::Infinity;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 0;
    die if     ref $self->[0];
    return "new PIL2JS.Box.Constant(Infinity)";
  }
}

{
  package PIL::NaN;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    my $self = shift;

    die unless @$self == 0;
    die if     ref $self->[0];
    return "new PIL2JS.Box.Constant(NaN)";
  }
}

1;
