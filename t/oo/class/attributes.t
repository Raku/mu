#!/usr/bin/pugs

use v6;
use Test;

plan 8;

=pod

Class Attributes

=cut

# L<S12/"Attributes">

eval_ok 'class Foo { our $.bar = 23; our $.yada is rw = 13; }', 'class attributes are parsed', :todo<feature>;

my $test = 0;
eval_ok '$test = Foo.bar', 'accessors for class attributes work', :todo<feature>;

is $test, 23, 'class attributes really work', :todo<feature>;

eval_ok 'class Baz is Foo {}', 'inheriting class attributes parsed', :todo<feature>;

my $test2 = 0;
eval_ok '$test2 = Baz.bar', 'inherited class attribute accessors work', :todo<feature>;

is $test2, 23, 'inherited class attributes really work', "todo<feature>;

my $test3 = 0;
eval_ok 'Baz.yada = 42; $test3 = Baz.yada', 'inherited rw class attribute accessors work', :todo<feature>;

is $test3, 42, 'inherited rw class attributes really work', "todo<feature>;
