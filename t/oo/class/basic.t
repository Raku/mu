#!/usr/bin/pugs

use v6;
use Test;

plan 8;
force_todo 1..3, 5, 7;

=pod

Very basic class tests from L<S12/"Classes">

=cut

# L<S12/"Classes">

class Foo {};

my $foo = Foo.new();
ok($foo ~~ Foo, '... smartmatch our $foo to the Foo class');

my $foo_clone = $foo.clone();
ok($foo_clone ~~ Foo, '... smartmatch our $foo_clone to the Foo class');

class Foo::Bar {};

my $foo_bar = Foo::Bar.new();
ok($foo_bar ~~ Foo::Bar, '... smartmatch our $foo_bar to the Foo::Bar class');

# L<S12/"Classes" /An \"isa\" is just a trait that happens to be another class\:/>

class Bar is Foo {};

ok(Bar ~~ Foo, '... smartmatch our Bar to the Foo class');

my $bar = Bar.new();
ok($bar ~~ Bar, '... smartmatch our $bar to the Bar class');
ok($bar ~~ Foo, '... smartmatch our $bar to the Foo class', :todo<feature>);

my $bar_clone = $bar.clone();
ok($bar_clone ~~ Bar, '... smartmatch our $bar_clone to the Bar class');
ok($bar_clone ~~ Foo, '... smartmatch our $bar_clone to the Foo class', :todo<feature>);
