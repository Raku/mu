# my $x = ...
package PIL::PPad;

use warnings;
use strict;

sub as_js {
  my $self = shift;

  die unless @$self == 3;
  die unless $self->[0]->isa("PIL::SMy");
  die unless ref $self->[1] eq "ARRAY";
  die unless $self->[2]->isa("PIL::PStmts");

  # Emit appropriate var foo = new PIL2JS.Box(undefined) statements.
  local $_;
  return
    "var " .
    join(", ", map {
      my $sigil = substr $_->[0], 0, 1;
      my %undef = (
        '$' => "undefined",
        '@' => "[]",
        '%' => "new PIL2JS.Hash()",
        '&' => "undefined",
      );
      PIL::name_mangle($_->[0]) .
      " = new PIL2JS.Box(@{[ $undef{$sigil} || die ]})"
    } @{ $self->[1] }) .
    ";\n" .
    $self->[2]->as_js;
}

1;
