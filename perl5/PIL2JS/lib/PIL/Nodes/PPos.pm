# PPos -- simply ignore the position and ->as_js the real contents.
# The position is stored in $CUR_POS -- that way PIL::PStmts can update
# $?POSITION accordingly.
{
  package PIL::PPos;

  use warnings;
  use strict;

  sub as_js {
    my $self = shift;
    die unless @$self == 3;
    die unless $self->[0]->isa("PIL::MkPos");

    local $CUR_POS = $self->[0];
    return $self->[2]->as_js;
  }
}

{
  package PIL::MkPos;

  use warnings;
  use strict;

  use overload '""' => \&as_string;

  sub as_string {
    my $self = shift;
    die unless @$self == 5;

    my ($file, $line_start, $column_start, $line_end, $column_end) = @$self;
    return "$file line $line_start-$line_end, column $column_start-$column_end";
  }
}

1;
