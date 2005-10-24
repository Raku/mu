#!/usr/bin/pugs

use v6;
use Test;

=pod

Class Attributes

=cut

# L<S12/"Attributes">

plan 8;

eval_ok 'class Foo { our $.bar = 23; our $.yada is rw = 13; }; 1', 'class attributes are parsed';

my $test = 0;
ok eval('$test = Foo.bar'), 'accessors for class attributes work', :todo<feature>;
is $test, 23, 'class attributes really work', :todo<feature>;

eval_ok 'class Baz is Foo {}; 1', 'inheriting class attributes parsed';

my $test2 = 0;
ok eval('$test2 = Baz.bar'), 'inherited class attribute accessors work', :todo<feature>;
is $test2, 23, 'inherited class attributes really work', :todo<feature>;

my $test3 = 0;
ok eval('Baz.yada = 42; $test3 = Baz.yada'), 'inherited rw class attribute accessors work', :todo<feature>;
is $test3, 42, 'inherited rw class attributes really work', :todo<feature>;
