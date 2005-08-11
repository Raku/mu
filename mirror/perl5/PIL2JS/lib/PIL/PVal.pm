use warnings;
use strict;

{
  package PIL::PVal;

  # xxx slighty hacky
  sub fixup {
    my $self = shift;
    local $_;

    if(
      ref $self eq "PIL::VUndef" or
      ref $self eq "PIL::Infinity" or
      ref $self eq "PIL::NaN"
    ) {
      die unless @$self == 0;
    } else {
      die unless @$self == 1;
    }

    if(ref $self eq "PIL::PVal" or ref $self eq "PIL::VNum") {
      die unless $self->[0]->isa("PIL::PVal");
      return bless [ $self->[0]->fixup ] => ref $self;
    } elsif(ref $self eq "PIL::VList") {
      return bless [ map { $_->fixup } @{ $self->[0] } ] => "PIL::VList";
    } elsif(ref $self eq "PIL::VBool") {
      die unless $self->[0]->isa("PIL::True") or $self->[0]->isa("PIL::False");
      return bless [ $self->[0] ] => "PIL::VBool";
    } else {
      die if ref $self->[0];
      return bless [ $self->[0] ] => ref $self;
    }
  }

  sub as_js { $_[0]->[0]->as_js }
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

  sub as_js { $_[0]->[0]->as_js } # XXX?
}

{
  package PIL::VRat;
  our @ISA = qw<PIL::PVal>;

  sub as_js { sprintf "new PIL2JS.Box.Constant(%s)", join "/", split "%", $_[0]->[0] }
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
      $_[0]->[0]->isa("PIL::True")  ? "true"  :
      $_[0]->[0]->isa("PIL::False") ? "false" : die;
  }
}

{
  package PIL::VList;
  our @ISA = qw<PIL::PVal>;

  sub as_js {
    local $_;
    return sprintf "new PIL2JS.Box.Constant([%s])",
      join ", ", map { $_->as_js } @{ $_[0]->[0] };
  }
}

{
  package PIL::VUndef;
  our @ISA = qw<PIL::PVal>;

  sub as_js { "new PIL2JS.Box.Constant(undefined)" }
}

{
  package PIL::Infinity;
  our @ISA = qw<PIL::PVal>;

  sub as_js { "new PIL2JS.Box.Constant(Infinity)" }
}

{
  package PIL::NaN;
  our @ISA = qw<PIL::PVal>;

  sub as_js { "new PIL2JS.Box.Constant(NaN)" }
}

1;
