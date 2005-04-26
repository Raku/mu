#!/usr/bin/pugs

use v6;
use Test;

=pod

Tests for the die() builtin

=cut

plan 3;

ok(!eval 'die "foo"; 1');
my $error = $!;
is($error, 'foo', 'got $! correctly');

my $foo = "-foo-";
eval '$foo = die "bar"';
$foo; # this is testing for a bug where an error is stored into $foo in
      # the above eval; unfortunately the if below doesn't detect this on it's
      # own, so this lone $foo will die if the bug is present
ok($foo eq "-foo-");
