#!/usr/bin/pugs

use v6;
require Test;

plan 4;

# things that should be valid
# these tests are probably going to fail if declaring a magical var ever becomes unallowed
ok((eval 'my $!; 1'), '$! parses ok');
ok((eval 'my $/; 1'), 'as does $/');

# things that should be invalid
ok(!(eval 'my $f!ao = "beh"; 1'), "but normal varnames can't have ! in their name");
todo_ok(!(eval 'my $fo:o::b:ar = "bla"; 1'), "var names can't have colons in their names either");

