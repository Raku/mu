# PPos -- simply ignore the position and ->as_js the real contents.
# The position is stored in $CUR_POS -- that way PIL::PStmts can update
# $?POSITION accordingly.
{
  package PIL::PPos;

  use warnings;
  use strict;

  sub fixup {
    die unless keys %{ $_[0] } == 3;
    die unless $_[0]->{pPos}->isa("PIL::MkPos");
    die if     $_[0]->{pExp};

    $_[0]->{pNode} = bless {} => "PIL::PNoop" if $_[0]->{pNode} eq "PNoop";

    return bless {
      pPos  => $_[0]->{pPos}->fixup,
      pExp  => undef,
      pNode => $_[0]->{pNode}->fixup,
    } => "PIL::PPos";
  }

  sub as_js {
    my $self = shift;

    local $PIL::CUR_POS = $self->{pPos};
    ($self->{pNode}{CC} and die) or $self->{pNode}{CC} = $self->{CC} if $self->{CC};

    return $self->{pNode}->as_js;
  }

  sub unwrap { $_[0]->{pNode}->unwrap }
}

{
  package PIL::MkPos;

  use warnings;
  use strict;

  use overload '""' => \&as_string;

  sub fixup {
    die unless keys %{ $_[0] } == 5;

    return bless {%{ $_[0] }} => "PIL::MkPos";
  }

  sub as_string {
    my ($file, $line_start, $column_start, $line_end, $column_end) =
      @{ $_[0] }{qw< posName posBeginLine posBeginColumn posEndLine posEndColumn>};
    return "$file line $line_start-$line_end, column $column_start-$column_end";
  }
}

1;
