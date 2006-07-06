# #!/usr/bin/pugs

use v6-alpha;

my $has_run = 0;
if 0 {  # XXX parser error - uncomment when fixed
eval '

say "1..3";

{
    my $string = "Pugs";
    if $string.ref eq Str { say "ok 1 # TODO" } else { say "not ok 1" }
}

{
    my $bool = ?1;
    if $bool.ref eq Bool { say "ok 2 # TODO" } else { say "not ok 2" }
}

{
    my $bool = Bool::True;
    if $bool.ref eq Bool { say "ok 3 # TODO" } else { say "not ok 3" }
}

$has_run = 1;
';
}
unless $has_run {
    say "1..1";
    say 'ok 1 # skip TODO parse Class bare-name as term';
}
