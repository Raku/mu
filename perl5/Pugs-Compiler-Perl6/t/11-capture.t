# #!/usr/bin/pugs
use v6-pugs;
use Test;

say "1..2";

my $a;

(+$a) := (:a<foo>);

if $a eq "foo" { say "ok 1" } else { say "not ok 1" }

sub set_a ($new_a) {
    $a = $new_a;
}

set_a("foo1");
if $a eq "foo1" { say "ok 2" } else { say "not ok 2" }

set_a(:a<foo2>);
if $a eq "foo2" { say "ok 3" } else { say "not ok 3" }
