#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::MetaModel;

=pod

This test file checks the details of the Class attribute accessor
generation, in particular it checks the following:

=over 4

=item private attributes do not get accessors

=item private attributes can still be reached inside the local class

=item public attributes do get accessors

=item public attributes do get mutators

=item public attributes mutators will change the attribute value

=back

=cut

class Base => {
    class => {
        attrs => [ '$:foo' ],
        init => sub { (shift)->set_value('$:foo' => 'Base::Foo') },
        methods => {
            get_base_foo => sub { (shift)->get_value('$:foo') },
            set_base_foo => sub { (shift)->set_value('$:foo' => 'Base::Foo -> new') }            
        }
    }
};

class Derived1 => {
    extends => 'Base',
    class => {
        attrs => [ '$.foo', '$:bar' ],
        init => sub { (shift)->set_value('$.foo' => 'Foo::Foo') },
    }
};

my $d = Derived1->new_instance();
isa_ok($d, 'Derived1');

ok(!$d->can('bar'), '... we cannot bar() because that is private');

is($d->foo(), 'Foo::Foo', '... the foo attribute was collected in the right order');
is($d->get_base_foo(), 'Base::Foo', '... the Base::foo attribute can still be accessed');

$@ = undef;
eval { $d->foo('New::Foo') };
ok(!$@, '... setting a public attribute did not fail');

is($d->foo(), 'New::Foo', '... the foo attribute can be changed with the accessor');

$@ = undef;
eval { $d->set_base_foo() };
ok(!$@, '... calling a method which sets a private attribute worked correctly');

is($d->get_base_foo(), 'Base::Foo -> new', '... the Base::foo attribute can still be accessed');

$@ = undef;
eval {
    $d->get_value('$:foo')
};
ok($@, '... getting a private value failed correctly');

$@ = undef;
eval {
    $d->set_value('$:foo' => 'nothing')
};
ok($@, '... setting a private value failed correctly');

# check for incorrect parameters

$@ = undef;
eval {
    $d->get_value('$.foo2')
};
ok($@, '... getting a incorrect parameter failed correctly');

$@ = undef;
eval {
    $d->set_value('$.foo2' => 'nothing')
};
ok($@, '... setting a incorrect parameter failed correctly');

# check for accessor conflicts

class ConflictChecker => {
    class => {
        attrs => [ '$.foo' ],
        init => sub { (shift)->set_value('$.foo' => 'just $.foo') },
        methods => {
            foo => sub {
                my $self = shift;
                'ConflictChecker->foo returns "' . $self->get_value('$.foo') . '"'
            }
        }
    }    
};

my $cc = ConflictChecker->new_instance();
isa_ok($cc, 'ConflictChecker');

is($cc->foo(), 'ConflictChecker->foo returns "just $.foo"', '... got the right value from the accessor');

# check for typed accessor

role Checker => {};

class TypeChecking => {
    does => [ 'Checker' ],
    class => {
        attrs => [ 
            [ 'TypeChecking', '$.foo' ],
            [ 'Checker',      '$.bar' ],
            [ 'Checkers',     '@.baz' ],      
            [ 'TypeChecking', '@.bah' ],                        
        ]
    }    
};

my $tc = TypeChecking->new_instance();
isa_ok($tc, 'TypeChecking');

my $tc2 = TypeChecking->new_instance();
isa_ok($tc2, 'TypeChecking');

$@ = undef;
eval { $tc->foo($tc2) };
ok(!$@, '... we do not have an exception (Class is correct type)');

$@ = undef;
eval { $tc->bar($tc2) };
ok(!$@, '... we do not have an exception (Role is correct type)');

$@ = undef;
eval { $tc->baz([ $tc2, $tc2, $tc2, $tc2 ]) };
ok(!$@, '... we do not have an exception (Roles are correct type)');

$@ = undef;
eval { $tc->bah([ $tc2, $tc2, $tc2, $tc2 ]) };
ok(!$@, '... we do not have an exception (Classes are correct type)');

is($tc->foo(), $tc2, '... and the value is as expected');

$@ = undef;
eval { $tc->foo('Fail') };
ok($@, '... we do have an exception when we try to assign a non-blessed type');

$@ = undef;
eval { $tc->foo($cc) };
ok($@, '... we do have an exception when we assign a blessed type of the wrong type');
