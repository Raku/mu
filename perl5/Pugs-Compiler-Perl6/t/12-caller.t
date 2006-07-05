use v6-**;

say "1..3";

my $a;

sub foo1 {
    my $x = &?ROUTINE;
    return $x.name;
}

sub foo2 {
    return &?ROUTINE.name;
}

sub foo3 {
    return caller().package;
}

sub foo3wrap {
    return foo3();
}

my $foo = foo1();

if ($foo eq '&main::foo1') { say "ok 1" } else { say "not ok 1" }

$foo = foo2();

if ($foo eq '&main::foo2') { say "ok 2" } else { say "not ok 2" }

# XXX: caller_cv doesn't work right with top-level main fake_cv
$foo = foo3wrap();

if ($foo eq 'main') { say "ok 3" } else { say "not ok 3" }
