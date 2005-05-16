#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/capture.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 65;

rule dotdot { (.)(.) };

ok("zzzabcdefzzz" ~~ m/(a.)<?dotdot>(..)/, 'Match');
ok($/, 'Matched');
is($/, "abcdef", 'Captured');
is($/[0], 'ab', '$/[0]');
is($0, 'ab', '$0');
is($/[1], 'ef', '$/[1]');
is($1, 'ef', '$1');
ok(!defined($/[2]), 'no $/[2]');
ok(!defined($2), 'no $2');
ok(!defined($/<dotdot>), 'no $/<dotdot>');

ok("zzzabcdefzzz" ~~ m/(a.)<dotdot>(..)/, 'Match');
ok($/, 'Matched');
is($/, "abcdef", 'Captured');
is($/[0], 'ab', '$/[0]');
is($0, 'ab', '$0');
is($/[1], 'ef', '$/[1]');
is($1, 'ef', '$1');
ok(!defined($/[2]), '$/[2]');
ok(!defined($2), '$2');
is($/<dotdot>, 'cd', '$/<dotdot>');
is($/<dotdot>[0], 'c', '$/<dotdot>[0]');

is($/<dotdot>[1], 'd', '$/<dotdot>[1]');

ok(!defined($/<dotdot>[2]), '$/<dotdot>[2]');

ok("abcd" ~~ m/(a(b(c))(d))/, 'Nested captured');
is($0, "abcd", 'Nested $0');
is($0[0], "bc", 'Nested $1');
is($0[0][0], "c", 'Nested $2');
is($0[1], "d", 'Nested $3');

ok("bookkeeper" ~~ m/(((\w)$0[0][0])+)/, 'Backreference');
is($0, 'ookkee', Captured );
is($0[0], 'ee', Captured );

rule single { o | k | e };

ok("bookkeeper" ~~ m/<single> ($/<single>)/, 'Named backref');
is($/<single>, 'o', 'Named capture');
is($0, 'o', 'Backref capture');

ok("bookkeeper" ~~ m/(<?single>) ($0)/, 'Positional backref');
is($0, 'o', 'Named capture');
is($1, 'o', 'Backref capture');

ok(!( "bokeper" ~~ m/(<?single>) ($0)/ ), 'Failed positional backref');
ok(!( "bokeper" ~~ m/<single> ($/<single>)/ ), 'Failed named backref');

is("\$0", '$'~'1', 'Non-translation of non-interpolated "\\$0"' );
is('$0',  '$'~'1', 'Non-translation of non-interpolated \'$0\'' );
is(q($0), '$'~'1', 'Non-translation of non-interpolated q($0)');
is(q{$0}, '$'~'1', 'Non-translation of non-interpolated q{$0}');
is(q[$0], '$'~'1', 'Non-translation of non-interpolated q[$0]');
is(q<$0>, '$'~'1', 'Non-translation of non-interpolated q<$0>');
ok(q<$0 <<<>>>> eq '$'~'1 <<<>>>', 'Non-translation of nested q<$0>');
is(q/$0/, '$'~'1', 'Non-translation of non-interpolated q/$0/');
is(q!$0!, '$'~'1', 'Non-translation of non-interpolated q!$0!');
is(q|$0|, '$'~'1', 'Non-translation of non-interpolated q|$0|');
is(q#$0#, '$'~'1', 'Non-translation of non-interpolated q#$0#');


grammar English { rule name { john } }
grammar French  { rule name { jean } }
grammar Russian { rule name { ivan } }

ok("john" ~~ m/<?English.name> | <?French.name> | <?Russian.name>/, 'English name');
is($/, "john", 'Match is john');
ok($/ ne "jean", "Match isn't jean");
is($/<name>, "john", 'Name is john');

ok("jean" ~~ m/<?English.name> | <?French.name> | <?Russian.name>/, 'French name');
is($/, "jean", 'Match is jean');
is($/<name>, "jean", 'Name is jean');

ok("ivan" ~~ m/<?English.name> | <?French.name> | <?Russian.name>/, 'Russian name');
is($/, "ivan", 'Match is ivan');
is($/<name>, "ivan", 'Name is ivan');

rule name { <?English.name> | <?French.name> | <?Russian.name> }
 
ok("john" ~~ m/<name>/, 'English metaname');
is($/, "john", 'Metaname match is john');
ok($/ ne "jean", "Metaname match isn't jean");
is($/<name>, "john", 'Metaname is john');
is($/<name><name>, "john", 'Metaname name is john');
