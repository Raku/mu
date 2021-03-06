use v6;

use Test;

=begin kwid

I/O tests

=end kwid

plan 4;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

# Part 2 can fail when tests are run concurrently.  This ensures part 2
# waits until part 1 is done before trying to read the file

sleep 0.1 until 'io_finalize_sync' ~~ :e;
unlink('io_finalize_sync');

my $filename = 'tempfile';

# Test is continued from io_finalized_part1.t
# Should see "Hello World" but with bug it is undef

my $fh = open($filename);
isa_ok($fh, 'IO');
my $line = get($fh);
is($line, "Hello World", 'finalize without explicit filehandle close worked');

#now be sure to delete the file as well

ok($fh.close, 'file closed okay');
ok(?unlink($filename), 'file has been removed');
