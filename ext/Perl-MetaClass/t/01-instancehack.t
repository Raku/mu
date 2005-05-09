#!/usr/bin/pugs

# test the instance hack

use v6;
use Test;

plan 6;

use_ok('Hack::Instances');

eval_ok '
  module My::Class;

  use Hack::Instances;

  sub My::Class::new returns Str is export {
	my %self = ( "value" => 3 );
	make_instance("My::Class", %self);
  };

  sub counter(Str $inv:) returns Int {
    my $self = get_instance($inv);
    my $value = ++$self<value>;
    return $value;
  };
1;
', '... our "Class" compiled ok';

my $object1 = My::Class::new();
my $object2 = My::Class::new();

is($object1.counter(), 4, '... $object1.counter() == 4');
is($object2.counter(), 4, '... $object2.counter() == 4');
is($object1.counter(), 5, '... $object1.counter() == 5');

dies_ok { 'Not an Instance'.count() }, '... check for correct errors';

