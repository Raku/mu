#!/usr/bin/pugs

use v6;
use Test;

plan 7;

=pod

Very basic class tests from L<S12/"Classes">

=cut

# L<S12/"Classes">

class Foo {};

my $foo = Foo.new();
ok($foo ~~ Foo, '... smartmatch our $foo to the Foo class');

my $foo_clone = $foo.clone();
ok($foo_clone ~~ Foo, '... smartmatch our $foo_clone to the Foo class');

# L<S12/"Classes" /An \"isa\" is just a trait that happens to be another class\:/>

eval 'class Bar is Foo {}';

eval_ok('Bar ~~ Foo', '... smartmatch our Bar to the Foo class', :todo<feature>);

my $bar = eval 'Bar.new()';
eval_ok('$bar ~~ Bar', '... smartmatch our $bar to the Bar class', :todo<feature>);
eval_ok('$bar ~~ Foo', '... smartmatch our $bar to the Foo class', :todo<feature>);

my $bar_clone = eval '$bar.clone()';
eval_ok('$bar_clone ~~ Bar', '... smartmatch our $bar_clone to the Bar class', :todo<feature>);
eval_ok('$bar_clone ~~ Foo', '... smartmatch our $bar_clone to the Foo class', :todo<feature>);
