use v6-alpha;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/context.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

skip_rest "This file was in t_disabled/.  Remove this SKIP when it works.";
=begin END

plan 7;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

my $str = "abcabcabc";
ok($str ~~ m:p/abc/, 'Continued match');
ok($str.pos == 3, 'Continued match pos');

$str = "abcabcabc";
my $x = $str ~~ m:i:p/abc/;
ok($str.pos == 3, 'Insensitive continued match pos');

$x = $str ~~ m:i:p/abc/;
ok($str.pos == 6, 'Insensitive recontinued match pos');

$str = "abcabcabc";
my @x = $str ~~ m:i:g:p/abc/;
is("@x", "abc abc abc", 'Insensitive repeated continued match');
ok($str.pos == 9, 'Insensitive repeated continued match pos');

$str = "abcabcabc";
@x = scalar $str ~~ m:p:i:g/abc/;
ok($str.pos == 3, 'Insensitive scalar repeated continued match pos');

}

