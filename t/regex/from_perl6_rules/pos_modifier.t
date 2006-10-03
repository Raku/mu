use v6-alpha;
use Test;

=pod

This file was originally derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/continue.t.

=cut

plan 6;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

force_todo(1,2,3,4,6);

for ("abcdef") {
    ok(m:pos/abc/, "Matched 1: '$/'" );
    eval_ok(q{.pos == 3}, 'Interim position correct');
    ok(m:pos/ghi|def/, "Matched 2: '$/'" );
    eval_ok(q{.pos == 6}, 'Final position correct');
}

my $_ = "foofoofoo foofoofoo";
ok(s:global:pos/foo/FOO/, 'Globally contiguous substitution');
is($_, "FOOFOOFOO foofoofoo", 'Correctly substituted contiguously');

}

