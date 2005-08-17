use warnings;
use strict;

{
  package PIL::TypeOr;

  use overload '""' => \&as_string;

  sub all_types { local $_; map { $_->all_types } @{ $_[0] } }

  sub matches {
    my ($self, $want) = @_;
    for($self->all_types) {
      return 1 == 1 if $_->matches($want);
    }
    return 1 == 0;
  }

  sub as_string { local $_; join "|", map { "(" . $_->as_string . ")" } @{ $_[0] } }
}

{
  package PIL::TypeAnd;

  use overload '""' => \&as_string;

  sub all_types { local $_; map { $_->all_types } @{ $_[0] } }

  sub matches {
    my ($self, $want) = @_;
    for($self->all_types) {
      return 1 == 0 if not $_->matches($want);
    }
    return 1 == 1;
  }

  sub as_string { local $_; join "&", map { "(" . $_->as_string . ")" } @{ $_[0] } }
}

{
  package PIL::MkType;

  use overload '""' => \&as_string;

  sub all_types { ($_[0]) }

  sub matches {
    my ($self, $want) = ($_[0]->[0], $_[1]);

    $want eq "Any" or $self eq $want;
  }

  sub as_string { $_[0]->[0] }
}

1;
