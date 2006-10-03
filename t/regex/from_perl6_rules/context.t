use v6-alpha;
use Test;

=pod

This file was originally derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/context.t.

=cut

plan 7;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

force_todo(1..7);

my $str = "abcabcabc";
ok($str ~~ m:p/abc/, 'Continued match');

eval_ok(q{$str.pos == 3}, 'Continued match pos');

$str = "abcabcabc";
my $x = $str ~~ m:i:p/abc/;
eval_ok(q{$str.pos == 3}, 'Insensitive continued match pos');

$x = $str ~~ m:i:p/abc/;
eval_ok(q{$str.pos == 6}, 'Insensitive recontinued match pos');

$str = "abcabcabc";
my @x = $str ~~ m:i:g:p/abc/;
is("@x", "abc abc abc", 'Insensitive repeated continued match');
eval_ok(q{$str.pos == 9}, 'Insensitive repeated continued match pos');

$str = "abcabcabc";
@x = ?($str ~~ m:p:i:g/abc/);
eval_ok(q{$str.pos == 3}, 'Insensitive scalar repeated continued match pos');

}

