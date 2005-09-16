#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 27;
use Test::Exception;

use Perl6::MetaModel;

=pod

This test is primarily focues on Role composition (resolving)

=cut

=pod

These should all resolves ok

role rFoo {
    has $.foo;
    method foo {}
}

role rBar {
    has $.bar;
    method bar {}
}

role rFooBar does rFoo does rBar {
    has $.foo_bar;
    method foo_bar {}
}

=cut

my $rFoo = $::Role->new('$:name' => 'rFoo');
$rFoo->add_method(foo => ::make_method(sub { 'rFoo::foo' }));
$rFoo->add_attribute('$.foo' => ::make_attribute('$.foo'));

my $rBar = $::Role->new('$:name' => 'rBar');
$rBar->add_method(bar => ::make_method(sub { 'rBar::bar' }));
$rBar->add_attribute('$.bar' => ::make_attribute('$.bar'));

my $rFooBar = $::Role->new('$:name' => 'rFooBar');
$rFooBar->roles([ $rFoo, $rBar ]);
$rFooBar->add_attribute('$.foo_bar' => ::make_attribute('$.foo_bar'));
$rFooBar->add_method(foo_bar => ::make_method(sub { 
    my $self = shift;
    ($self->foo() . '/' . $self->bar());
}));

ok($rFoo->does('rFoo'), '... $rFoo does rFoo');
ok($rBar->does('rBar'), '... $rBar does rBar');

ok($rFooBar->does('rBar'), '... $rFooBar does rBar');
ok($rFooBar->does('rFoo'), '... $rFooBar does rFoo');
ok($rFooBar->does('rFooBar'), '... $rFooBar does rFooBar');

is_deeply(
    $rFooBar->roles,
    [ $rFoo, $rBar ],
    '... $rFooBar has the right subroles');

is_deeply(
    [ $rFooBar->get_method_list ],
    [ 'foo_bar' ],
    '... got the right (unresolved) method list for FooBar');
    
is_deeply(
    [ $rFooBar->get_attribute_list ],
    [ '$.foo_bar' ],
    '... got the right (unresolved) attribute list for FooBar');    

my $rFooBar_flat = $rFooBar->resolve();

is_deeply(
    [ sort $rFooBar_flat->get_method_list ],
    [ 'bar', 'foo', 'foo_bar' ],
    '... got the right (resolved) method list for FooBar');
    
is_deeply(
    [ sort $rFooBar_flat->get_attribute_list ],
    [ '$.bar', '$.foo', '$.foo_bar' ],
    '... got the right (resolved) attribute list for FooBar');    
    
is(scalar(grep { $rFooBar_flat->is_method_stub($_) } $rFooBar_flat->get_method_list), 
   0, '... we have no method stubs');

=pod

as should these ...

role rBaz {
    has $.baz;
    method baz {}
}

role rFooBarBaz does rFooBar does rBaz {
    has $.foo_bar_baz;
    method foo_bar_baz {}
}

=cut

my $rBaz = $::Role->new('$:name' => 'rBaz');
$rBaz->add_method(baz => ::make_method(sub { 'rBaz::baz' }));
$rBaz->add_attribute('$.baz' => ::make_attribute('$.baz'));

my $rFooBarBaz = $::Role->new('$:name' => 'rFooBarBaz');
$rFooBarBaz->roles([ $rFooBar, $rBaz ]);
$rFooBarBaz->add_attribute('$.foo_bar_baz' => ::make_attribute('$.foo_bar_baz'));
$rFooBarBaz->add_method(foo_bar_baz => ::make_method(sub { 
    my $self = shift;
    ($self->foo . '/' . $self->bar . '/' . $self->baz);
}));

ok($rBaz->does('rBaz'), '... $rBaz does rBaz');

ok($rFooBarBaz->does('rFoo'), '... $rFooBarBaz does rFoo');
ok($rFooBarBaz->does('rBar'), '... $rFooBarBaz does rBar');
ok($rFooBarBaz->does('rFooBar'), '... $rFooBarBaz does rFooBar');
ok($rFooBarBaz->does('rBaz'), '... $rFooBarBaz does rBaz');
ok($rFooBarBaz->does('rFooBarBaz'), '... $rFooBarBaz does rFooBarBaz');

is_deeply(
    $rFooBarBaz->roles,
    [ $rFooBar, $rBaz ],
    '... $rFooBarBaz has the right subroles');

is_deeply(
    [ $rFooBarBaz->get_method_list ],
    [ 'foo_bar_baz' ],
    '... got the right (unresolved) method list for FooBarBaz');
    
is_deeply(
    [ $rFooBarBaz->get_attribute_list ],
    [ '$.foo_bar_baz' ],
    '... got the right (unresolved) attribute list for FooBarBaz');    

my $rFooBarBaz_flat = $rFooBarBaz->resolve();

is_deeply(
    [ sort $rFooBarBaz_flat->get_method_list ],
    [ 'bar', 'baz', 'foo', 'foo_bar', 'foo_bar_baz' ],
    '... got the right (resolved) method list for FooBarBaz');

is_deeply(
    [ sort $rFooBarBaz_flat->get_attribute_list ],
    [ '$.bar', '$.baz', '$.foo', '$.foo_bar', '$.foo_bar_baz' ],
    '... got the right (resolved) attribute list for FooBarBaz');   
    
is(scalar(grep { $rFooBarBaz_flat->is_method_stub($_) } $rFooBarBaz_flat->get_method_list), 
   0, '... we have no method stubs');    

=pod

This only resolves if the roles and subroles (recursively) are 
linearized and all the duplicates are removed. This results in 
a clean list of roles which all come together nicely. 

If you were to instead use rFooBar and rFooBarBaz as composite
roles to be combined together, then they would conflict with 
each other. However, this does not make sense for this to happen
because the conflicts are happening with the same subroles (the
rFoo and rBar and rFooBar all exist as subroles in rFooBarBaz, 
and rFoo and rBar exist in rFooBar). It does not make sense that
these roles would conflict with flattened versions of themselves. 

role rFahShizzle does rFooBar does rFooBarBaz {}

=cut

my $rFahShizzle = $::Role->new('$:name' => 'rFahShizzle');
$rFahShizzle->roles([ $rFooBar, $rFooBarBaz ]);

my $rFahShizzle_flat = $rFahShizzle->resolve();

is_deeply(
    [ sort $rFahShizzle_flat->get_method_list ],
    [ 'bar', 'baz', 'foo', 'foo_bar', 'foo_bar_baz' ],
    '... got the right (resolved) method list for rFahShizzle');
    
is_deeply(
    [ sort $rFahShizzle_flat->get_attribute_list ],
    [ '$.bar', '$.baz', '$.foo', '$.foo_bar', '$.foo_bar_baz' ],
    '... got the right (resolved) attribute list for rFahShizzle');    

is(scalar(grep { $rFahShizzle_flat->is_method_stub($_) } $rFahShizzle_flat->get_method_list), 
   0, '... we have 3 method stubs');     
    
is(scalar(grep { $rFahShizzle_flat->is_attribute_stub($_) } $rFahShizzle_flat->get_attribute_list), 
   0, '... we have 3 attribute stubs');     
    