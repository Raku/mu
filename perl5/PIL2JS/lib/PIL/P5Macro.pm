package PIL::P5Macro::JS;

use warnings;
use strict;

sub new {
  my ($class, $body) = @_;

  return bless { body => $body } => $class;
}

sub as_js {
  my ($self, @args) = @_;

  return $self->{body}->($self->{CC}, @args);
}

1;
