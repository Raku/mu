#!/usr/bin/pugs

use v6;
use Test;

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
undefine $foo;
ok(!$foo, 'undef');

my $bar;
unless ($foo) { $bar = "true" }
ok($bar, "unless");

my ($var1, $var2) = ("foo", "bar");
is($var1, "foo", 'list assignment 1');
is($var2, "bar", 'list assignment 2');
ok(eval('(my $quux = 1) == 1)'), "my() returns LHS", :todo);

lives_ok({ if 1 { 1 } }, "if without parens");
lives_ok({ for 1 { 1 } }, "for without parens");
lives_ok({ while (0) { 0 } }, "while");

my $lasttest = 0;
for (1..10) { $lasttest++; last; $lasttest++; }
ok($lasttest == 1, "last");

my $nexttest = 0;
for (1..10) { $nexttest++; next; $nexttest++; }
ok($nexttest == 10, "next");

is(12.eval, 12, "12.eval");
is(eval(1 ?? 1 !! 0), 1, "?? !!");

ok({ my $_ = 1; $_ }, '{ my $_ = 1; $_ }');
