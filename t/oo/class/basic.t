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
todo_eval_ok('$foo ~~ Foo', '... smartmatch our $foo to the Foo class');

my $foo_clone = eval '$foo.clone()';
todo_eval_ok('$foo_clone ~~ Foo', '... smartmatch our $foo_clone to the Foo class');

# L<S12/"Classes" /An \"isa\" is just a trait that happens to be another class\:/>

eval 'class Bar is Foo {}';

todo_eval_ok('Bar ~~ Foo', '... smartmatch our Bar to the Foo class');

my $bar = eval 'Bar.new()';
todo_eval_ok('$bar ~~ Bar', '... smartmatch our $bar to the Bar class');
todo_eval_ok('$bar ~~ Foo', '... smartmatch our $bar to the Foo class');

my $bar_clone = eval '$bar.clone()';
todo_eval_ok('$bar_clone ~~ Bar', '... smartmatch our $bar_clone to the Bar class');
todo_eval_ok('$bar_clone ~~ Foo', '... smartmatch our $bar_clone to the Foo class');