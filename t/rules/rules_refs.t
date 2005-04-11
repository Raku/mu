#!/usr/bin/pugs

use v6;
require Test;

plan 3;

=pod

Test for rules as references

=cut

my $rule = rx:perl5{\s+};
isa_ok($rule, 'Rule');

ok("hello world" ~~ $rule, '... applying rule ref returns true');
ok(!("helloworld" ~~ $rule), '... applying rule ref returns false (correctly)');
