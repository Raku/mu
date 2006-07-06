# #!/usr/bin/pugs
 
use v6-alpha;

say "1..2";

{
    my $code = { 42 };
    if $code.isa("Code")  { say "ok 1" } else { say "not ok 1" }
}

{
    my $code = sub { 42 };
    if $code.isa("Code")  { say "ok 2" } else { say "not ok 2" }
}

# just testing if this compiles
sub { 42 }
