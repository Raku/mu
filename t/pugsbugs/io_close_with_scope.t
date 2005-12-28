#!/usr/bin/pugs

use v6;
use Test;

=kwid

I/O tests

=cut

plan 2;

if $*OS eq "browser" {
	skip_rest "Programs running in browsers don't have access to regular IO.";
	exit;
}

my $filename = "foo";

{
	my $fh = open($filename, :w);
	isa_ok($fh, IO);
}

{
	my $fh = open($filename, :w);
	isa_ok($fh, IO);
}
