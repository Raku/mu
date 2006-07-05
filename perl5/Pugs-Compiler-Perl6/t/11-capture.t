
use v6-pugs;

say "1..4";

my $a;

#(+$a) := (:a<foo>);

if $a eq "foo" { say "ok 1" } else { say "not ok 1 # TODO binding not yet" }

sub set_a ($new_a) {
    $a = $new_a;
}

set_a("foo1");
if $a eq "foo1" { say "ok 2" } else { say "not ok 2" }

set_a(:new_a<foo2>);
if $a eq "foo2" { say "ok 3" } else { say "not ok 3" }

sub set_a2 ($useless, $new_a) {
    $a = $new_a;
}

my $new_a = 'foo3';
set_a2(:useless<blah>, :$new_a);
if $a eq "foo3" { say "ok 4" } else { say "not ok 4" }
