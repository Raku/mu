use v6;

require Test;
exit;

plan(16);

#todo_fail("FIXME parsefail");
say "1 not ok - FIXME parsefail # TODO";

eval '
my foo = 1;
if ( foo == 1) { say "1 ok" } else { say "1 not ok" }
if ($foo == 1) { say "2 ok" } else { say "2 not ok" }

foo++
if ( foo == 2) { say "3 ok" } else { say "3 not ok" }
if ($foo == 2) { say "4 ok" } else { say "4 not ok" }

$foo++
if ( foo == 3) { say "5 ok" } else { say "5 not ok" }
if ($foo == 3) { say "6 ok" } else { say "6 not ok" }

sub bar { 42 }
if (bar == 42) { say "13 ok" } else { say "13 not ok" }

my bar = 24;  # should probably warn
if ( bar   == 24) { say "14 ok" } else { say "14 not ok" } # warn again?
if ($bar   == 24) { say "15 ok" } else { say "15 not ok" }
if (&bar() == 42) { say "16 ok" } else { say "16 not ok" } # cannot work if
                                                           # this whole thing is
                                                           # implemented with
                                                           # an lvalue sub...
';
