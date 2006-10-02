use v6-alpha;

use Test;

=pod

Class Attributes

=cut

#L<S12/Attributes/"Class attributes are declared">
#L<S12/Class methods/metaclass method always delegated>

plan 17;

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

eval_ok 'class Quux is Foo { has $.bar = 17; }; 1',
    'overriding with instance method allowed';
my $test4 = 0;
ok eval('$test4 = Quux.new()'),
    'Can instantiate with overridden instance method';
is $test4.bar, 17, 'Instance call gets instance attribute, not class attribute';
my $test5 = 0;
ok eval('$test5 = Quux.bar'), 'class attribute still accessible via class name', :todo<feature>;
is $test5, 23, 'class attribute really works, even when overridden', :todo<feature>;
my $test6 = 0;
ok eval('$test6 = Quux.^bar'), 'class attribute accessible via ^name', :todo<feature>;
is $test6, 23, 'class attribute via ^name really works', :todo<feature>;
my $test7 = 0;
ok eval('$test7 = $test4.^bar'),
    'class attribute accessible via ^name called on instance', :todo<feature>;
is $test7, 23, 'class attribute via $obj.^name really works', :todo<feature>;
