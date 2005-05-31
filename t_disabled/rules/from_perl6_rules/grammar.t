#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/grammar.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 17;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

grammar Other {
	rule abc { a (<?bee>) c }

	rule bee { b }

	rule def { d <eh> f }

	rule eh  { e }
}

rule bee { B }

ok('abc' ~~ m/^ (<?Other.abc>) $/, '<?Other.abc>');
is($/, "abc", 'abc $/');
is($0, "abc", 'abc $0');

ok('abc' ~~ m/ (<?Other.bee>) /, '<?Other.bee>');
is($/, "b", 'bee $/');
is($0, "b", 'bee $0');

ok('def' ~~ m/^ (<?Other.def>) $/, '(<?Other.def>)');
is($/, "def", 'def $/');
is($0, "def", 'def $0');

ok('def' ~~ m/^ <Other.def> $/, '<Other.def>');
is($/, "def", '?def $/');
ok($0 ne "def", '?def $0');
is($/<def>, "def", '?def $/<def>');
is($/<def><eh>, "e", '?def $/<def><eh>');

ok(!( 'abc' ~~ m/ (<?bee>) / ), '<?bee>');

ok(!eval { 'abc' ~~ m/ (<?Other.sea>) / }, '<?Other.sea>');
ok($!, 'Error');

}

