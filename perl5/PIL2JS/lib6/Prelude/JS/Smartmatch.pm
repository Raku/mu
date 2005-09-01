sub infix:«~~» (Item $left, Item $right) is primitive {
  if $right.isa(::Rul) {
    $right.matcher.($left);
  } elsif $right.isa(Str) {
    $left eq $right;
  }
}

sub infix:«!~» (Item $left, Item $right) is primitive { not $left ~~ $right }
