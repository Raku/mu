#!/usr/bin/pugs

# test the instance hack

use v6;
use Test;

plan 10;

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

