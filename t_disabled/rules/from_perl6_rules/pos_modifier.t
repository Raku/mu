#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/continue.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 6;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

for ("abcdef") {
	ok(m:pos/abc/, "Matched 1: '$/'" );
	ok(.pos == 3, 'Interim position correct');
	ok(m:pos/ghi|def/, "Matched 2: '$/'" );
	ok(.pos == 6, 'Final position correct');
}

my $_ = "foofoofoo foofoofoo";
ok(s:global:pos/foo/FOO/, 'Globally contiguous substitution');
is($_, "FOOFOOFOO foofoofoo", 'Correctly substituted contiguously');

}

