use v6-alpha;

say "1..3";

{
    my $code = { 42 };
    if $code.isa("Code")  { say "ok 1" } else { say "not ok 1" }
}

{
    my $code = sub { 42 };
    if $code.isa("Code")  { say "ok 2" } else { say "not ok 2" }
    if $code() == 42   { say "ok 3" } else { say "not ok 3" }
}

