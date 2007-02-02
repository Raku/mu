use v6-alpha;

use Test;

=pod

  This tests parsing of filetest operators

=cut


plan 3;

# only "~~:f" should be parsed as the filetest operator, not "- f"
sub f { return 8; }

ok($*PROGRAM_NAME ~~ :f, "~~:f returns true on files");
is(f($*PROGRAM_NAME), 8, "f(...) works");
is(- f($*PROGRAM_NAME), -8, "- f(...) does not call the ~~:f filetest");
