#!/usr/bin/pugs

# test the instance hack

use v6;
use Test;

use_ok('Hack::Instances');

make_class("My::Class");

sub counter(Str $inv:) returns Int {
    return -1;
}

eval_ok '
  module My::Class;

  use Hack::Instances;

  sub My::Class::new returns My::Class is export {
    return make_instance("My::Class");
  }

  sub counter(My::Class $inv:) returns Int {
    my $self = get_instance($inv);

    return ++$self<value>;
  }
';

my $object1 = My::Class::new();
my $object2 = My::Class::new();

is($object1.counter(), 1, ".counter()");
is($object2.counter(), 1, ".counter()");
is($object1.counter(), 2, ".counter()");

