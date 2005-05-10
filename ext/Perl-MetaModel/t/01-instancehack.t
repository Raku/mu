#!/usr/bin/pugs

# test the instance hack

use v6;
use Test;

plan 22;

use_ok('Hack::Instances');

eval_ok '
  module My::Class;

  use Hack::Instances;

  sub My::Class::new returns Str is export {
	my %self = ( "value" => 3 );
	make_instance("My::Class", %self);
  };

  sub counter(Str $inv:) returns Int {
    my $self = get_instance($inv, "My::Class");
    my $value = ++$self<value>;
    return $value;
  };
1;
', '... our "Class" compiled ok';

my $object1 = My::Class::new();
ok($object1.instance_isa('My::Class'), '... this is a My::Class instance');

ok(!$object1.instance_isa('Foo::Bar'), '... this is not a Foo::Bar instance');

my $object2 = My::Class::new();
ok($object2.instance_isa('My::Class'), '... this is a My::Class instance');

is($object1.counter(), 4, '... $object1.counter() == 4');
is($object2.counter(), 4, '... $object2.counter() == 4');
is($object1.counter(), 5, '... $object1.counter() == 5');

dies_ok { 'Not an Instance'.count() }, '... check for correct errors';
dies_ok { get_instance($object1, "Not::My::Class"); }, '... check for correct errors';

ok(!"Not an instance".blessed(), "... check 'blessed' method (negative)");
ok(!"OBJECT;Martian;12345".blessed(), "... check 'blessed' method (negative - fake)");
ok($object1.blessed(), "... check 'blessed' method (positive)");

ok(!1.blessed(), "... check 'blessed' method (negative - Num)");
ok(!{}.blessed(), "... check 'blessed' method (negative - \{})");
my $sub = sub { return (1) };
ok(!$sub.blessed(), "... check 'blessed' method (negative - sub)");

ok(!1.can("counter"), "... check 'can' method (negative - Num)");
ok(!"OBJECT;My::Class;1234".can("counter"), "... check 'can' method (negative - fake)");
is($object1.can("counter"), &counter, "... check 'can' method (positive)");

is(1.class(), undef, "... check 'class' method (negative - Num)");
is("OBJECT;My::Class;1234".class(), undef, "... check 'class' method (negative - fake)");
is($object1.class(), "My::Class", "... check 'class' method (positive)");
