use v6;

use Test;

plan 2;

=begin pod

A class can derive another class regardless of the order in which they're
written in the file.

=end pod

lives_ok { class A {}; class B is A {}; }, "base before derived";
lives_ok { class D is C {}; class C {}; }, "derived before base";
