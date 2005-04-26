#!/usr/bin/pugs

use v6;
use Test;

plan 3;


my $fh = open $*PROGRAM_NAME;
ok($fh, "could open self");
isa_ok($fh, 'IO');

my $line;
eval '
	$fh is chomped;
	$line = <$fh>;
';

is($line, "#!/usr/bin/pugs", "first line was chomped", :todo(1));
