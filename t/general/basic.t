#!/usr/bin/pugs

use v6;
require Test;

=kwid

Basic tests.

=cut

plan 15;

ok(1, "Welcome to Pugs!");

sub cool { ok(fine($_), " # We've got " ~ toys) }
sub fine { $_ == 2 }
sub toys { "fun and games!" }

(2).cool;  # and that is it, folks!

my $foo = "Foo";
eval 'undef $foo';
ok(!$foo, 'undef');

my $bar;
unless ($foo) { $bar = "true" }
ok($bar, "unless");

my ($var1, $var2) = ("foo", "bar");
is($var1, "foo", 'list assignment 1');
is($var2, "bar", 'list assignment 2');
todo_ok(eval '(my $quux = 1) == 1)', "my() returns LHS");

ok(eval 'if 1 { 1 }; 1', "if without parens");
ok(eval 'for 1 { 1 }; 1', "for without parens");
ok(eval 'while (0) { 0 }; 1', "while");

my $lasttest = 0;
eval 'for (1..10) { $lasttest++; last; $lasttest++; }; 1';
todo_ok($lasttest == 1, "last");

my $nexttest = 0;
eval 'for (1..10) { $nexttest++; next; $nexttest++; }; 1';
todo_ok($nexttest == 10, "next");

ok(eval '12.eval', "12.eval");
ok(eval 'eval(1 ?? 1 :: 0)', "?? ::");

ok(eval '{ my $_ = 1; $_ }', '{ my $_ = 1; $_ }');
