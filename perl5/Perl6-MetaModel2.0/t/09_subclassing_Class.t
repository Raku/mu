#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 39;
use Test::Exception; 

use Perl6::MetaModel;

my $CountedClass = $::Class->new('$:name' => 'CounterClass');
isa_ok($CountedClass, 'CounterClass');

$CountedClass->superclasses([ $::Class ]);

isa_ok($CountedClass, 'Class');
isa_ok($CountedClass, 'Object');

$CountedClass->add_attribute('$:count', ::make_attribute('$:count'));
ok($CountedClass->has_attribute('$:count'), '... the attribute was added successfully');

$CountedClass->add_method('new' => ::make_method(sub {
    ::opaque_instance_attrs($::SELF)->{'$:count'}++;
    return ::next_METHOD();
}));

ok($CountedClass->has_method('new'), '... the new method was overridden successfully');

$CountedClass->add_method('count' => ::make_method(sub {
    ::opaque_instance_attrs($::SELF)->{'$:count'};
}));

ok($CountedClass->has_method('count'), '... the count method was added successfully');

## create some CountedClass classes

my $Foo = $CountedClass->new('$:name' => 'Foo');
isa_ok($Foo, 'Foo');

can_ok($Foo, 'count');
is($Foo->count(), undef, '... we have no Foo instances');

$Foo->superclasses([ $::Object ]);
$Foo->add_method('bar' => ::make_method(sub { 'Foo::bar' }));

ok($Foo->has_method('bar'), '... Foo is a proper Class, and can get methods added');

my $iFoo = $Foo->new();
isa_ok($iFoo, 'Foo');

is($Foo->count(), 1, '... we have one Foo instance');

can_ok($iFoo, 'bar');
is($iFoo->bar(), 'Foo::bar', '... got the right value from bar()');

foreach my $count (2 .. 5) {
    $Foo->new();
    is($Foo->count(), $count, '... we have ' . $count . ' Foo instances');    
}


## now test creating the Class subclass with the meta-model

my $TraceingClass = class 'TracingClass' => {
    is => [ $::Class ],
    attributes => [ '@:trace_log' ],
    methods => {
        'get_trace_log' => sub { _('@:trace_log') },
        'add_method' => sub {
            my ($self, $label, $method) = @_;
            $_[2] = ::make_method(sub {  
                push @{::opaque_instance_attrs($self)->{'@:trace_log'}} => "$label called ...";
                $method->(@_);
            });
            ::next_METHOD();
        }
    }
};
isa_ok($TraceingClass, 'TracingClass');
isa_ok($TraceingClass, 'Class');
isa_ok($TraceingClass, 'Object');

## create a TraceingClass by hand ...

my $Bar = $TraceingClass->new('$:name' => 'Bar');
$Bar->superclasses([ $::Object ]);

isa_ok($Bar, 'Bar');
isa_ok($Bar, 'Object');

is_deeply(
    $Bar->get_trace_log(),
    [],
    '... nothing in the Bar trace log yet');

$Bar->add_method('foo' => ::make_method(sub { 'Bar::foo' }));
$Bar->add_method('baz' => ::make_method(sub { 'Bar::baz' }));

# and another to show it is truely a per-class thing

## create a TraceingClass with the metamodel

my $Baz = class 'Baz' => {
    metaclass => $TraceingClass,
    is => [ $::Object ],
    methods => {
        'bar' => sub { 'Baz::bar' },
        'foo' => sub { 'Baz::foo' },
    }
};

isa_ok($Baz, 'Baz');
isa_ok($Baz, 'Object');

is_deeply(
    $Baz->get_trace_log(),
    [],
    '... nothing in the Baz trace log yet');

## now to make some instances

my $iBar = $Bar->new();
isa_ok($iBar, 'Bar');

my $iBaz = $Baz->new();
isa_ok($iBaz, 'Baz');

is_deeply(
    $Bar->get_trace_log(),
    [],
    '... still nothing in the Bar trace log yet (new() is inherited)');

is_deeply(
    $Baz->get_trace_log(),
    [],
    '... still nothing in the Baz trace log yet (new() is inherited)');

is($iBar->foo(), 'Bar::foo', '... iBar->foo returned the correct output');
is($iBaz->bar(), 'Baz::bar', '... iBaz->bar returned the correct output');

is_deeply(
    $Bar->get_trace_log(),
    [ 'foo called ...' ],
    '... we now have a Bar trace log');

is_deeply(
    $Baz->get_trace_log(),
    [ 'bar called ...' ],
    '... we now have a Bar trace log');

is($iBar->baz(), 'Bar::baz', '... iBar->baz returned the correct output');
is($iBaz->foo(), 'Baz::foo', '... iBaz->foo returned the correct output');

is_deeply(
    $Bar->get_trace_log(),
    [ 'foo called ...', 'baz called ...' ],
    '... we now have more in our Bar trace log');

is_deeply(
    $Baz->get_trace_log(),
    [ 'bar called ...', 'foo called ...' ],
    '... we now have more in our Bar trace log');

# TracingClass has some GC issues,.. 
# so we need to manually remove it here
# these are mostly because perl does not
# do any kind of ordered destruction
END {
    $Perl6::MetaModel::CLASSES_BY_NAME{'TraceingClass'} = undef;
    delete $Perl6::MetaModel::CLASSES_BY_NAME{'TraceingClass'};
    $TraceingClass = undef;
}
