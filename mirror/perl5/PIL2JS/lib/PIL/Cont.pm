package PIL::Cont;

use warnings;
use strict;

sub new { bless { @_[1..$#_] } => $_[0] }

sub as_js {
  return sprintf "(function (%s) {\n%s\n})",
    $_[0]->{argname},
    PIL::add_indent(
      1,
      ref $_[0]->{body} eq "CODE"
        ? $_[0]->{body}->()
        : $_[0]->{body}->as_js
    );
}

1;
