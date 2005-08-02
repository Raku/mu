#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=pod

Test for rules as references

=cut

my $rule = rx:perl5{\s+};
isa_ok($rule, 'Pugs::Internals::VRule');

ok("hello world" ~~ $rule, '... applying rule ref returns true');
ok(!("helloworld" ~~ $rule), '... applying rule ref returns false (correctly)');
