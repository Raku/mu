#!/usr/bin/pugs

use v6;
require Test;

plan 7;

=pod

Very basic class tests from L<S12/"Classes">

=cut

# L<S12/"Classes">

eval 'class Foo {}';

my $foo = eval 'Foo.new()';
eval_ok('$foo ~~ Foo', '... smartmatch our $foo to the Foo class', :todo(1));

my $foo_clone = eval '$foo.clone()';
eval_ok('$foo_clone ~~ Foo', '... smartmatch our $foo_clone to the Foo class', :todo(1));

# L<S12/"Classes" /An \"isa\" is just a trait that happens to be another class\:/>

eval 'class Bar is Foo {}';

eval_ok('Bar ~~ Foo', '... smartmatch our Bar to the Foo class', :todo(1));

my $bar = eval 'Bar.new()';
eval_ok('$bar ~~ Bar', '... smartmatch our $bar to the Bar class', :todo(1));
eval_ok('$bar ~~ Foo', '... smartmatch our $bar to the Foo class', :todo(1));

my $bar_clone = eval '$bar.clone()';
eval_ok('$bar_clone ~~ Bar', '... smartmatch our $bar_clone to the Bar class', :todo(1));
eval_ok('$bar_clone ~~ Foo', '... smartmatch our $bar_clone to the Foo class', :todo(1));
