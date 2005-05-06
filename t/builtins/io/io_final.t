#!/usr/bin/pugs

use v6;
use Test;

=kwid

I/O finalize test

=cut

plan 4;

my $filename = 'tempfile';

# This is a continued test from io.t
# A file was written to but not closed
# We are testing that the output actually got to the file

my $in = open($filename);
isa_ok($in, 'IO');
my $line = readline($in);
is($line, "Hello World\n", 'writing file without explicit close worked');
ok($in.close, 'file closed okay');


#now be sure to delete the file as well

ok(?unlink($filename), 'file has been removed');

