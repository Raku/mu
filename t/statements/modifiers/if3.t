use v6-alpha;

use Test;
plan 1;

sub foo() {
  return if 1;
  123;
}

for foo() -> @foo {
    die "oops";
}
ok 1, "ok 1\n";
