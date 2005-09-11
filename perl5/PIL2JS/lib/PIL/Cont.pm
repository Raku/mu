package PIL::Cont;

use warnings;
use strict;

sub new { bless { @_[1..$#_] } => $_[0] }

sub as_js {
  no warnings "recursion";
  return sprintf "(function (%s) {\n%s%s\n})",
    $_[0]->{argname},
    defined $_[1] ? PIL::add_indent(1, "$_[1]\n") : "",
    PIL::add_indent(
      1,
      ref $_[0]->{body} eq "CODE"
        ? $_[0]->{body}->()
        : $_[0]->{body}->as_js
    );
}

1;
