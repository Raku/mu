#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 35;
use Test::Exception;

use Perl6::MetaModel;

use_ok('Perl6::Container::Scalar');

# test basic

my $perl6_scalar;
tie $perl6_scalar, 'Perl6::Container::Scalar';

ok(tied($perl6_scalar), '... out scalar is tied');
isa_ok(tied($perl6_scalar), 'Perl6::Container::Scalar');

lives_ok {
    $perl6_scalar = 'testing';
} '... STORE the value correctly';

ok(tied($perl6_scalar), '... out scalar is still tied');
is($perl6_scalar, 'testing', '... and the value is what we added to it');

# test with initial value

my $perl6_scalar_w_default;
tie $perl6_scalar_w_default, 'Perl6::Container::Scalar', { default => 42 };

ok(tied($perl6_scalar_w_default), '... out scalar is tied');
isa_ok(tied($perl6_scalar_w_default), 'Perl6::Container::Scalar');

is($perl6_scalar_w_default, 42, '... and the default value is what we gave it');

lives_ok {
    $perl6_scalar_w_default = 'testing';
} '... STORE the value correctly';

ok(tied($perl6_scalar_w_default), '... out scalar is still tied');
is($perl6_scalar_w_default, 'testing', '... and the value has been changed');

# test typed scalar w/ classes

my $perl6_typed_scalar;
tie $perl6_typed_scalar, 'Perl6::Container::Scalar', { type => 'MyObject' };

ok(tied($perl6_typed_scalar), '... out typed scalar is tied');
isa_ok(tied($perl6_typed_scalar), 'Perl6::Container::Scalar');

class MyObject => {};
my $my_obj = MyObject->new();

lives_ok {
    $perl6_typed_scalar = $my_obj;
} '... STORE the typed value correctly';

ok(tied($perl6_typed_scalar), '... out scalar is still tied');
is($perl6_typed_scalar, $my_obj, '... and the value is what we added to it');

dies_ok {
    $perl6_typed_scalar = 'testing';
} '... STORE failed correctly (typed scalar being assigned a non-blessed type)';

dies_ok {
    $perl6_typed_scalar = bless {}, 'Fail';
} '... STORE failed correctly (typed scalar being assigned a blessed type of the wrong type)';

# test typed scalar w/ classes #2 (type is subclass of value added)

my $perl6_typed_scalar2;
tie $perl6_typed_scalar2, 'Perl6::Container::Scalar', { type => 'MyNewObject' };

ok(tied($perl6_typed_scalar2), '... out typed scalar is tied');
isa_ok(tied($perl6_typed_scalar2), 'Perl6::Container::Scalar');

class MyNewObject => {
    is => [ 'MyObject' ]
};

lives_ok {
    $perl6_typed_scalar2 = $my_obj;
} '... STORE the typed value correctly';

ok(tied($perl6_typed_scalar2), '... out scalar is still tied');
is($perl6_typed_scalar2, $my_obj, '... and the value is what we added to it');

# test typed scalar w/ classes w/ default

my $perl6_typed_scalar3;
lives_ok {
    tie $perl6_typed_scalar3, 'Perl6::Container::Scalar', { type => 'MyNewObject', default => $my_obj };
} '... assigning a typed default works correctly';

ok(tied($perl6_typed_scalar3), '... out typed scalar is tied');
isa_ok(tied($perl6_typed_scalar3), 'Perl6::Container::Scalar');

is($perl6_typed_scalar3, $my_obj, '... and it is the default value we gave it');

# test typed scalar w/ classes w/ default w/error

my $perl6_typed_scalar4;
dies_ok {
    tie $perl6_typed_scalar4, 'Perl6::Container::Scalar', { type => 'SomeOtherObject', default => $my_obj };
} '... assigning a typed default fails correctly';

## ROLES

# test typed scalar w/ classes

my $perl6_typed_scalar_w_roles;
tie $perl6_typed_scalar_w_roles, 'Perl6::Container::Scalar', { type => 'MyRole' };

ok(tied($perl6_typed_scalar_w_roles), '... out typed scalar is tied');
isa_ok(tied($perl6_typed_scalar_w_roles), 'Perl6::Container::Scalar');

role MyRole => {};
class SomeObject => {
    does => [ 'MyRole' ]
};

my $some_obj = SomeObject->new();

lives_ok {
    $perl6_typed_scalar_w_roles = $some_obj;
} '... STORE the typed value correctly';

ok(tied($perl6_typed_scalar_w_roles), '... out scalar is still tied');
is($perl6_typed_scalar_w_roles, $some_obj, '... and the value is what we added to it');

role MyOtherRole => {};
class BadObject => {
    does => [ 'MyOtherRole' ]
};

dies_ok {
    $perl6_typed_scalar_w_roles = BadObject->new();
} '... STORE failed correctly (typed scalar being assigned a blessed type of the wrong role type)';
