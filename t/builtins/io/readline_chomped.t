#!/usr/bin/pugs

use v6;
use Test;

plan 3;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}


my $fh = open $*PROGRAM_NAME;
ok($fh, "could open self");
isa_ok($fh, 'IO');

my $line;
eval '
  $fh is chomped;
  $line = =$fh;
';

is($line, "#!/usr/bin/pugs", "first line was chomped", :todo);
