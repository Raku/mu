use v6-pugs;

say "1..2";

my $a;

sub foo1 {
    my $x = &?ROUTINE;
    return $x.name;
}

sub foo2 {
    return &?ROUTINE.name;
}

my $foo = foo1();

if ($foo eq '&main::foo1') { say "ok 1" } else { say "not ok 1" }

$foo = foo2();

if ($foo eq '&main::foo2') { say "ok 2" } else { say "not ok 2" }
