# PPos -- simply ignore the position and ->as_js the real contents.
# The position is stored in $CUR_POS -- that way PIL::PStmts can update
# $?POSITION accordingly.
{
  package PIL::PPos;

  use warnings;
  use strict;

  sub fixup {
    die unless @{ $_[0] } == 3;
    die unless $_[0]->[0]->isa("PIL::MkPos");
    die unless $_[0]->[1]->isa("PIL::Noop"); # minor hack

    return bless [
      $_[0]->[0]->fixup,
      $_[0]->[1],
      $_[0]->[2]->fixup,
    ] => "PIL::PPos";
  }

  sub as_js {
    local $PIL::CUR_POS = $_[0]->[0];
    return $_[0]->[2]->as_js;
  }
}

{
  package PIL::MkPos;

  use warnings;
  use strict;

  use overload '""' => \&as_string;

  sub fixup {
    die unless @{ $_[0] } == 5;

    return bless [@{ $_[0] }] => "PIL::MkPos";
  }

  sub as_string {
    my ($file, $line_start, $column_start, $line_end, $column_end) = @{ $_[0] };
    return "$file line $line_start-$line_end, column $column_start-$column_end";
  }
}

1;
