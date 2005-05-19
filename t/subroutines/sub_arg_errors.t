#!/usr/bin/pugs

use v6;
use Test;

plan 5;

=pod

These are misc. sub argument errors.

=cut

sub foo (*$x) { 1 }
dies_ok  { foo(reverse(1,2)) }, 'slurpy args are now bounded (1)';

sub bar (*@x) { 1 }
lives_ok { bar(reverse(1,2)) }, 'slurpy args are now bounded (2)';

# try to flatten the args for baz() to match

sub baz ($a, $b) { return "a: $a b: $b"}
sub invoke (*@args) { baz(*@args) }

my $val;
lives_ok {
    $val = invoke(1, 2);
}, '... slurpy args flattening and matching parameters', :todo<bug>;

is($val, 'a: 1 b: 2', '... slurpy args flattening and matching parameters', :todo<bug>);

# try to flatten the args for the anon sub to match

sub invoke2 ($f, *@args) { $f(*@args) }; 
is(invoke2(sub ($a, $b) { return "a: $a b: $b"}, 1, 2, 3), 'a: 1 b: 2', 
    '... slurpy args flattening and matching parameters', :todo<bug>);
    