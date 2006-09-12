use v6-alpha;
use Test;

# L<S16>
# merge this with open.t?

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

is($line, "use v6-alpha;", "first line was chomped");
